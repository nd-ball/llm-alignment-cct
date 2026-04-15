#install.packages("here")
#install.packages("devtools")
#install_github('alastair-JL/AnthroTools')

library(devtools)
#library(here)
library(AnthroTools)

# ---------------- CONFIG ----------------
base_dir    <- here("ARR_March", "Data")
output_dir <- here("ARR_March", "Output")

domains_list <- c("HWB", "POC", "POM", "POS", "EV", "RV", "EVN", "POST", "SCTOM/I", "SCTOM/II", "SCTOM/III",
                  "SVNS/I", "SVNS/II", "SVNS/III", "PIPP/I", "PIPP/II", "PCPR/I", "PCPR/II")
#domains_list <- c("SCTOM/II")
countries   <- c("Armenia", "Germany", "Greece", "Japan", "Netherlands", "Colombia", "Malaysia", "Mexico", "Peru", "United_States")

llm_tail <- 60  
sanitize_for_filename <- function(x) gsub('[\\\\/:*?"<>| ]', "_", x)

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

# CCT metrics (competence + VE + consensus answers) 
cct_stats <- function(mat) {
  if (is.null(mat) || nrow(mat) == 0 || ncol(mat) == 0)
    return(list(comp = numeric(0), ve = NA_real_, er = NA_real_))
  if (nrow(mat) == 1)
    return(list(comp = 1, ve = 1, er = NA_real_))
  
  arr <- array(as.matrix(mat), dim = dim(mat))
  A <- MakeAgreementMatrix(arr)
  eg <- eigen(A)
  
  ve <- eg$values[1] / sum(eg$values)
  er <- if (length(eg$values) >= 2 && abs(eg$values[2]) > .Machine$double.eps) eg$values[1]/eg$values[2] else NA_real_
  
  f1 <- eg$vectors[, 1]
  comp <- abs(f1 / sqrt(sum(f1^2)))  
  
  list(comp = comp, ve = ve, er = as.numeric(er))
}

#RAW competence-weighted consensus
safe_weights <- function(w, n) {
  if (length(w) != n) return(rep(1, n))
  w[!is.finite(w)] <- 0
  if (all(w <= 0)) return(rep(1, n))
  w
}

# Function to calculate consensus using competence weights
weighted_item_consensus <- function(mat, w) {
  if (is.null(mat) || nrow(mat) == 0 || ncol(mat) == 0) return(numeric(0))
  w <- safe_weights(w, nrow(mat))
  vapply(seq_len(ncol(mat)), function(j) stats::weighted.mean(mat[, j], w = w, na.rm = TRUE), numeric(1))
}

for (domain in domains_list) {
  domain_safe <- sanitize_for_filename(domain)
  out_dir <- file.path(output_dir, "summaries", "CC_VE")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  out_csv <- file.path(out_dir, sprintf("CC_VE_%s.csv", domain_safe))
  
  rows <- list()
  
  for (cty in countries) {
    cty_dir <- file.path(base_dir, domain, cty)
    csv_path <- find_country_csv(cty_dir)
    if (is.null(csv_path) || !file.exists(csv_path)) {
      message(sprintf("[SKIP] Missing human_10LLMs.csv under: %s", cty_dir))
      next
    }
    
    df <- read_pair_csv(csv_path)
    if (is.null(df) || nrow(df) == 0) next
    
    labels <- if ("D_INTERVIEW" %in% names(df)) as.character(df$D_INTERVIEW) else rep("", nrow(df))
    
    cols <- setdiff(names(df), "D_INTERVIEW")
    df_num <- df[, cols, drop = FALSE]
    df_num[] <- lapply(df_num, function(x) suppressWarnings(as.numeric(as.character(x))))
    
    # Filling empty values in LLM (60 rows) with MODE
    df_num <- fill_tail_na_with_row_mode(df_num, tail_n = llm_tail)
    
    # drop remaining NA rows in Human responses
    keep <- stats::complete.cases(df_num)
    df_num <- df_num[keep, , drop = FALSE]
    labels <- labels[keep]
    
    if (nrow(df_num) == 0) {
      message(sprintf("[SKIP] %s became empty after NA removal.", cty))
      next
    }
    
    n_total <- nrow(df_num)
    n_llm   <- min(llm_tail, n_total)
    n_human <- max(n_total - n_llm, 0)
    
    human_idx <- if (n_human > 0) seq_len(n_human) else integer(0)
    llm_idx   <- if (n_llm > 0) seq.int(n_total - n_llm + 1, n_total) else integer(0)
    
    hum <- cct_stats(if (length(human_idx)) df_num[human_idx, , drop = FALSE] else df_num[0, , drop = FALSE])
    llm <- cct_stats(if (length(llm_idx))   df_num[llm_idx, , drop = FALSE]   else df_num[0, , drop = FALSE])
    
    # --- LOCAL Q99 SCALING LOGIC ---
    hum_raw <- if (length(hum$comp)) hum$comp * sqrt(n_human) else numeric(0)
    llm_raw <- if (length(llm$comp)) llm$comp * sqrt(n_llm) else numeric(0)
    
    # Calculate local Q99 for human
    q99_human_local <- NA_real_
    if (length(hum_raw) > 0) {
      q99_human_local <- as.numeric(quantile(hum_raw, probs = 0.99, na.rm = TRUE, type = 8))
      if (!is.finite(q99_human_local) || q99_human_local <= 0) q99_human_local <- max(hum_raw, na.rm = TRUE)
    }
    
    # Calculate local Q99 for LLM
    q99_llm_local <- NA_real_
    if (length(llm_raw) > 0) {
      q99_llm_local <- as.numeric(quantile(llm_raw, probs = 0.99, na.rm = TRUE, type = 8))
      if (!is.finite(q99_llm_local) || q99_llm_local <= 0) q99_llm_local <- max(llm_raw, na.rm = TRUE)
    }
    
    # Apply scaling to the local anchor and cap at 1.0
    hum_q99 <- if (length(hum_raw)) pmin(hum_raw / q99_human_local, 1) else numeric(0)
    llm_q99 <- if (length(llm_raw)) pmin(llm_raw / q99_llm_local, 1) else numeric(0)
    
    llm_labels <- if (length(llm_idx)) labels[llm_idx] else character(0)
    
    # LLM strings (all 60)
    llm_ids_str <- if (length(llm_labels)) paste0(llm_labels, collapse = ";") else ""
    llm_raw_vals_str <- if (length(llm$comp)) paste0(round(llm$comp, 6), collapse = ";") else ""
    llm_q99_vals_str <- if (length(llm_q99)) paste0(round(llm_q99, 6), collapse = ";") else ""
    
    llm_raw_map_str <- if (length(llm_labels) && length(llm$comp) == length(llm_labels)) {
      paste0(llm_labels, ":", round(llm$comp, 6), collapse = ";")
    } else ""
    llm_q99_map_str <- if (length(llm_labels) && length(llm_q99) == length(llm_labels)) {
      paste0(llm_labels, ":", round(llm_q99, 6), collapse = ";")
    } else ""
    
    # RAW competence weights and percent agreement
    hum_mat <- if (n_human > 0) df_num[human_idx, , drop = FALSE] else df_num[0, , drop = FALSE]
    llm_mat <- if (n_llm   > 0) df_num[llm_idx,   , drop = FALSE] else df_num[0, , drop = FALSE]
    
    hum_cons <- weighted_item_consensus(hum_mat, hum$comp)
    llm_cons <- weighted_item_consensus(llm_mat, llm$comp)
    
    human_consensus_str <- if (length(hum_cons)) paste0(round(hum_cons, 6), collapse = ";") else ""
    llm_consensus_str   <- if (length(llm_cons)) paste0(round(llm_cons, 6), collapse = ";") else ""
    
    pct_agreement_consensus <- if (length(hum_cons) && length(llm_cons) && length(hum_cons) == length(llm_cons)) {
      mean(round(hum_cons) == round(llm_cons), na.rm = TRUE) * 100
    } else {
      NA_real_
    }
    
    # Getting average competence for each LLM (10 records)
    llm_strided_vec <- rep(NA_real_, 10)
    if (length(llm_q99) == 60) {
      mat_strided <- matrix(llm_q99, ncol = 10, byrow = TRUE)
      llm_strided_vec <- colMeans(mat_strided, na.rm = TRUE)
    }    
    
    # Human row
    rows[[length(rows) + 1]] <- data.frame(
      domain = domain, country = cty, group = "human",
      source_csv = csv_path,
      n_group = n_human,
      eig_ratio = hum$er, variance_explained = hum$ve,
      comp_raw_mean = if (length(hum$comp)) mean(hum$comp, na.rm = TRUE) else NA_real_,
      comp_q99_mean = if (length(hum_q99)) mean(hum_q99, na.rm = TRUE) else NA_real_,
      q99_local_anchor_human = q99_human_local, q99_local_anchor_llm = q99_llm_local,
      llm_ids = "", llm_comp_raw_values = "", llm_comp_q99_values = "",
      llm_comp_raw_map = "", llm_comp_q99_map = "",
      human_consensus_raw_values = human_consensus_str,
      llm_consensus_raw_values   = llm_consensus_str,
      pct_agreement_consensus    = pct_agreement_consensus,
      GPT_OSS_120B = NA, LLAMA3_1_70B = NA, LLAMA3_70B = NA, PHI3_INSTRUCT = NA, QWEN2_5VL_32B = NA,
      QWEN2_5VL_72B = NA, QWEN2_5VL_7B = NA, QWEN3_32B = NA, QWEN_7B = NA, GPT_4O = NA,
      stringsAsFactors = FALSE
    )
    
    # LLM row 
    rows[[length(rows) + 1]] <- data.frame(
      domain = domain, country = cty, group = "llm",
      source_csv = csv_path,
      n_group = n_llm,
      eig_ratio = llm$er, variance_explained = llm$ve,
      comp_raw_mean = if (length(llm$comp)) mean(llm$comp, na.rm = TRUE) else NA_real_,
      comp_q99_mean = if (length(llm_q99)) mean(llm_q99, na.rm = TRUE) else NA_real_,
      q99_local_anchor_human = q99_human_local, q99_local_anchor_llm = q99_llm_local,
      llm_ids = llm_ids_str,
      llm_comp_raw_values = llm_raw_vals_str,
      llm_comp_q99_values = llm_q99_vals_str,
      llm_comp_raw_map = llm_raw_map_str,
      llm_comp_q99_map = llm_q99_map_str,
      human_consensus_raw_values = human_consensus_str,
      llm_consensus_raw_values   = llm_consensus_str,
      pct_agreement_consensus    = pct_agreement_consensus,
      GPT_OSS_120B = llm_strided_vec[1], LLAMA3_1_70B = llm_strided_vec[2], 
      LLAMA3_70B = llm_strided_vec[3], PHI3_INSTRUCT = llm_strided_vec[4], 
      QWEN2_5VL_32B = llm_strided_vec[5], QWEN2_5VL_72B = llm_strided_vec[6], 
      QWEN2_5VL_7B = llm_strided_vec[7], QWEN3_32B = llm_strided_vec[8], 
      QWEN_7B = llm_strided_vec[9], GPT_4O = llm_strided_vec[10],      
      stringsAsFactors = FALSE
    )
  }
  
  if (length(rows) > 0) {
    out_df <- do.call(rbind, rows)
    out_df <- out_df[order(out_df$country, out_df$group), ]
    utils::write.csv(out_df, out_csv, row.names = FALSE)
    message(sprintf("[DONE] Wrote %d rows to: %s", nrow(out_df), out_csv))
  } else {
    message(sprintf("[SKIP] No valid data found for domain: %s", domain))
  }
}