#install.packages("here")
#install.packages("devtools")
#install_github('alastair-JL/AnthroTools')

library(devtools)
#library(here)
library(AnthroTools)

# ---------------- CONFIG ----------------
#base_dir    <- here("ARR_March", "Data")
#output_dir <- here("ARR_March", "Output")

base_dir    <- "C:/Users/kpoth/Downloads/CCT/FEB16/Data"
output_dir <- "C:/Users/kpoth/Downloads/CCT/FEB16/Old_Code/MOD/Output"

domains_list <- c(
  "HWB", "POC", "POM", "POS", "EV", "RV", "EVN", "POST",
  "SCTOM/I", "SCTOM/II", "SCTOM/III",
  "SVNS/I", "SVNS/II", "SVNS/III",
  "PIPP/I", "PIPP/II",
  "PCPR/I", "PCPR/II"
)
# domains_list <- c("POST")

countries <- c(
  "Armenia", "Germany", "Greece", "Japan", "Netherlands",
  "Colombia", "Malaysia", "Mexico", "Peru", "United_States"
)
# countries <- c("Armenia")

llm_tail <- 60

sanitize_for_filename <- function(x) gsub('[\\\\/:*?"<>| ]', "_", x)

prompt_levels <- c("baseline","chainofthought","declarative","explicit","parantheses","roleplay")

extract_prompt <- function(id) {
  id <- trimws(id)
  p <- tolower(sub("^.*_([^_]+)$", "\\1", id))   # last token after _
  ifelse(p %in% prompt_levels, p, NA_character_)
}

strip_prompt <- function(id) {
  id <- trimws(id)
  p <- extract_prompt(id)
  ifelse(is.na(p), id, sub(paste0("_", p, "$"), "", id, ignore.case = TRUE))
}

read_pair_csv <- function(path) {
  tryCatch({
    if (requireNamespace("data.table", quietly = TRUE)) {
      data.table::fread(path, data.table = FALSE)
    } else {
      read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    }
  }, error = function(e) {
    message(sprintf("[SKIP] Failed to read: %s (%s)", path, e$message))
    NULL
  })
}

find_country_csv <- function(cty_dir) {
  hits <- list.files(cty_dir, pattern = "human_10LLMs\\.csv$", recursive = TRUE, full.names = TRUE)
  if (!length(hits)) return(NULL)
  if (length(hits) > 1) {
    message(sprintf("[WARN] Multiple human_10LLMs.csv found under %s; using first:\n  %s", cty_dir, hits[1]))
  }
  hits[1]
}

fill_tail_na_with_row_mode <- function(df, tail_n = 60) {
  n <- nrow(df); if (n == 0) return(df)
  tail_n_eff <- min(tail_n, n)
  tail_idx <- seq.int(n - tail_n_eff + 1, n)
  tail_has_na  <- anyNA(df[tail_idx, , drop = FALSE])
  other_has_na <- if (n > tail_n_eff) anyNA(df[-tail_idx, , drop = FALSE]) else FALSE
  
  if (tail_has_na && !other_has_na) {
    all_numeric <- all(vapply(df, is.numeric, logical(1)))
    
    for (i in tail_idx) {
      if (all_numeric) {
        x <- as.numeric(df[i, , drop = TRUE]); x <- x[!is.na(x)]
        if (!length(x)) next
        mode_val <- as.numeric(names(which.max(table(x))))
      } else {
        row_vals <- unlist(df[i, , drop = FALSE], use.names = FALSE)
        x <- row_vals[!is.na(row_vals)]
        if (!length(x)) next
        ux <- unique(x)
        mode_val <- ux[which.max(tabulate(match(x, ux)))]
      }
      
      na_cols <- which(is.na(df[i, ]))
      if (length(na_cols)) df[i, na_cols] <- mode_val
    }
    
    message(sprintf("[IMPUTE] Filled NAs in last %d rows using row-wise mode", tail_n_eff))
  } else if (tail_has_na && other_has_na) {
    message("[WARN] NAs also exist outside the last tail rows; skipping tail-only imputation.")
  }
  df
}

# ---------------------------
# CCT METRICS
# ---------------------------
# NOTE: requires MakeAgreementMatrix() to be available in your environment.
# Fixed CCT stats to force symmetric eigen decomposition.
cct_stats <- function(mat) {
  if (is.null(mat) || nrow(mat) == 0 || ncol(mat) == 0)
    return(list(comp = numeric(0), ve = NA_real_, er = NA_real_))
  if (nrow(mat) == 1)
    return(list(comp = 1, ve = 1, er = NA_real_))
  
  arr <- array(as.matrix(mat), dim = dim(mat))
  A <- MakeAgreementMatrix(arr)
  
  eg <- eigen(A, symmetric = TRUE)
  
  ve <- eg$values[1] / sum(eg$values)
  er <- if (length(eg$values) >= 2 && abs(eg$values[2]) > .Machine$double.eps) {
    eg$values[1] / eg$values[2]
  } else NA_real_
  
  f1 <- eg$vectors[, 1]
  comp <- abs(f1)
  
  list(comp = comp, ve = ve, er = as.numeric(er))
}

# ---------------------------
# CONSENSUS FUNCTIONS
# ---------------------------
safe_weights <- function(w, n) {
  if (length(w) != n) return(rep(1, n))
  w[!is.finite(w)] <- 0
  if (all(w <= 0)) return(rep(1, n))
  w
}

weighted_item_consensus <- function(mat, w) {
  if (is.null(mat) || nrow(mat) == 0 || ncol(mat) == 0) return(numeric(0))
  w <- safe_weights(w, nrow(mat))
  vapply(seq_len(ncol(mat)), function(j) stats::weighted.mean(mat[, j], w = w, na.rm = TRUE), numeric(1))
}

# Robust Q99 anchor helper (fallback to max if quantile is non-finite/<=0)
q99_anchor <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  q <- as.numeric(quantile(x, probs = 0.99, na.rm = TRUE, type = 8))
  if (!is.finite(q) || q <= 0) q <- max(x, na.rm = TRUE)
  q
}

# ---------------------------
# MAIN LOOP (LOCAL PER-COUNTRY Q99 ANCHORS)
# ---------------------------
for (domain in domains_list) {
  
  domain_safe <- sanitize_for_filename(domain)
  out_dir <- file.path(output_dir, "summaries", "Ablation", "CC_VE_PROMPTED")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  out_csv_combined <- file.path(out_dir, sprintf("CCT_METRICS_COMBINED_%s.csv", domain_safe))
  
  stash <- list()
  
  # ----- Read and compute stats per country; stash results -----
  for (cty in countries) {
    
    cty_dir <- file.path(base_dir, domain, cty)
    csv_path <- find_country_csv(cty_dir)
    if (is.null(csv_path) || !file.exists(csv_path)) next
    
    df <- read_pair_csv(csv_path)
    if (is.null(df) || nrow(df) == 0) next
    
    labels <- if ("D_INTERVIEW" %in% names(df)) as.character(df$D_INTERVIEW) else rep("", nrow(df))
    cols <- setdiff(names(df), "D_INTERVIEW")
    df_num <- df[, cols, drop = FALSE]
    df_num[] <- lapply(df_num, function(x) suppressWarnings(as.numeric(as.character(x))))
    
    df_num <- fill_tail_na_with_row_mode(df_num, tail_n = llm_tail)
    keep <- stats::complete.cases(df_num)
    df_num <- df_num[keep, , drop = FALSE]
    labels <- labels[keep]
    
    if (nrow(df_num) == 0) next
    
    n_total <- nrow(df_num)
    n_llm   <- min(llm_tail, n_total)
    n_human <- max(n_total - n_llm, 0)
    
    human_idx <- if (n_human > 0) seq_len(n_human) else integer(0)
    llm_idx   <- if (n_llm > 0) seq.int(n_total - n_llm + 1, n_total) else integer(0)
    
    hum <- cct_stats(if (length(human_idx)) df_num[human_idx, , drop = FALSE] else df_num[0, , drop = FALSE])
    
    llm_labels_all <- if (length(llm_idx)) labels[llm_idx] else character(0)
    llm_prompt_all <- extract_prompt(llm_labels_all)
    
    llm_idx_by_prompt <- setNames(lapply(prompt_levels, function(p) {
      if (!length(llm_idx)) return(integer(0))
      llm_idx[llm_prompt_all == p]
    }), prompt_levels)
    
    llm_by_prompt <- list()
    for (p in prompt_levels) {
      idxp <- llm_idx_by_prompt[[p]]
      if (!length(idxp)) next
      llm_by_prompt[[p]] <- cct_stats(df_num[idxp, , drop = FALSE])
    }
    
    stash[[cty]] <- list(
      n_human = n_human,
      hum = hum,
      llm_idx_by_prompt = llm_idx_by_prompt,
      llm_by_prompt = llm_by_prompt,
      df_num = df_num,
      human_idx = human_idx
    )
  }
  
  if (!length(stash)) next
  
  rows_combined <- list()
  
  # ----- Write rows with LOCAL (per-country) Q99 anchors -----
  for (cty in names(stash)) {
    s <- stash[[cty]]
    
    # --- LOCAL Q99 ANCHORS FOR THIS COUNTRY ---
    hum_raw_local <- if (s$n_human > 0 && length(s$hum$comp)) s$hum$comp * sqrt(s$n_human) else numeric(0)
    q99_human_local <- q99_anchor(hum_raw_local)
    
    # --- Human consensus answer key & string ---
    human_mat <- s$df_num[s$human_idx, , drop = FALSE]
    hum_cons <- if (nrow(human_mat) > 0) weighted_item_consensus(human_mat, s$hum$comp) else numeric(ncol(s$df_num))
    human_consensus_str <- if (length(hum_cons)) paste0(round(hum_cons, 6), collapse = ";") else ""
    
    # 1) Human Row
    comp_q99_mean_h <- NA_real_
    if (s$n_human > 0 && length(s$hum$comp)) {
      if (!is.na(q99_human_local) && q99_human_local > 0) {
        comp_q99_mean_h <- mean(pmin((s$hum$comp * sqrt(s$n_human)) / q99_human_local, 1), na.rm = TRUE)
      }
      
      rows_combined[[length(rows_combined) + 1]] <- data.frame(
        domain = domain, country = cty, group = "human", prompt = "N/A",
        n_group = s$n_human,
        variance_explained = s$hum$ve,
        eig_ratio = s$hum$er,
        comp_raw_mean = mean(s$hum$comp, na.rm = TRUE),
        comp_q99_mean = comp_q99_mean_h,
        human_consensus_str = human_consensus_str,
        llm_consensus_str = NA_character_,
        pct_agreement_consensus = NA_real_,
        stringsAsFactors = FALSE
      )
    }
    
    # 2) LLM Rows per Prompt (LOCAL per-country-per-prompt Q99 anchors)
    for (p in prompt_levels) {
      idxp <- s$llm_idx_by_prompt[[p]]
      llm_stats <- s$llm_by_prompt[[p]]
      
      if (!length(idxp) || is.null(llm_stats) || !length(llm_stats$comp)) next
      
      # local Q99 anchor for this country + this prompt
      llm_raw_local <- llm_stats$comp * sqrt(length(idxp))
      q99p_local <- q99_anchor(llm_raw_local)
      
      # LLM consensus string & agreement
      llm_mat <- s$df_num[idxp, , drop = FALSE]
      llm_cons <- weighted_item_consensus(llm_mat, llm_stats$comp)
      llm_consensus_str <- if (length(llm_cons)) paste0(round(llm_cons, 6), collapse = ";") else ""
      
      pct_agreement_consensus <- if (length(hum_cons) && length(llm_cons) && length(hum_cons) == length(llm_cons)) {
        mean(round(hum_cons) == round(llm_cons), na.rm = TRUE) * 100
      } else {
        NA_real_
      }
      
      comp_q99_mean_l <- NA_real_
      if (!is.na(q99p_local) && q99p_local > 0) {
        comp_q99_mean_l <- mean(pmin((llm_stats$comp * sqrt(length(idxp))) / q99p_local, 1), na.rm = TRUE)
      }
      
      rows_combined[[length(rows_combined) + 1]] <- data.frame(
        domain = domain, country = cty, group = "llm", prompt = p,
        n_group = length(idxp),
        variance_explained = llm_stats$ve,
        eig_ratio = llm_stats$er,
        comp_raw_mean = mean(llm_stats$comp, na.rm = TRUE),
        comp_q99_mean = comp_q99_mean_l,
        human_consensus_str = human_consensus_str,
        llm_consensus_str = llm_consensus_str,
        pct_agreement_consensus = pct_agreement_consensus,
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(rows_combined) > 0) {
    out_df <- do.call(rbind, rows_combined)
    out_df <- out_df[order(out_df$country, out_df$group, out_df$prompt), ]
    utils::write.csv(out_df, out_csv_combined, row.names = FALSE)
    message(sprintf("[DONE] Wrote consolidated metrics to: %s", out_csv_combined))
  }
}