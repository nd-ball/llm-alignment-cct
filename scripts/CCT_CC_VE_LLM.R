library(devtools)
#library(here)
library(AnthroTools)

base_dir    <- "C:/Users/kpoth/Downloads/CCT/FEB16/Data"
output_dir  <- "C:/Users/kpoth/Downloads/CCT/FEB16/Old_Code/MOD/Output"

domains_list <- c(
  "HWB", "POC", "POM", "POS", "EV", "RV", "EVN", "POST",
  "SCTOM/I", "SCTOM/II", "SCTOM/III",
  "SVNS/I", "SVNS/II", "SVNS/III",
  "PIPP/I", "PIPP/II",
  "PCPR/I", "PCPR/II"
)

countries <- c(
  "Armenia", "Germany", "Greece", "Japan", "Netherlands",
  "Colombia", "Malaysia", "Mexico", "Peru", "United_States"
)

llm_tail <- 60  # last 60 rows are LLM (6 prompts x 10 models)

sanitize_for_filename <- function(x) gsub('[\\\\/:*?"<>| ]', "_", x)

# =========================
# IO HELPERS
# =========================
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

# =========================
# CCT METRICS
# =========================
cct_stats <- function(mat) {
  if (is.null(mat) || nrow(mat) == 0 || ncol(mat) == 0)
    return(list(comp = numeric(0), ve = NA_real_, er = NA_real_))
  if (nrow(mat) == 1)
    return(list(comp = 1, ve = 1, er = NA_real_))
  
  arr <- array(as.matrix(mat), dim = dim(mat))
  A <- MakeAgreementMatrix(arr)
  eg <- eigen(A)
  
  ve <- eg$values[1] / sum(eg$values)
  er <- if (length(eg$values) >= 2 && abs(eg$values[2]) > .Machine$double.eps) eg$values[1] / eg$values[2] else NA_real_
  
  f1 <- eg$vectors[, 1]
  comp <- abs(f1 / sqrt(sum(f1^2)))
  
  list(comp = comp, ve = ve, er = as.numeric(er))
}

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

# =========================
# NEW: 10 base models (each has 6 prompt-respondents)
# NOTE: your requested stride logic: model j uses positions j, j+10, ..., j+50 in the LLM tail
# =========================
llm_model_base <- c(
  "GPT-OSS_120B",
  "LLAMA3_1_70B",
  "LLAMA3_70B",
  "PHI3_INSTRUCT",
  "QWEN2_5VL_32B",
  "QWEN2_5VL_72B",
  "QWEN2_5VL_7B",
  "QWEN3_32B",
  "QWEN_7B",
  "GPT_4O"
)

# columns already used in your output (underscores)
llm_model_cols <- gsub("-", "_", llm_model_base)

# =========================
# MAIN LOOP
# =========================
for (domain in domains_list) {
  domain_safe <- sanitize_for_filename(domain)
  out_dir <- file.path(output_dir, "summaries", "Ablation", "CC_VE_LLMLEVEL")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  out_csv <- file.path(out_dir, sprintf("CC_VE_LLMLevel_%s.csv", domain_safe))
  
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
    
    # Fill empty values in LLM (last 60 rows) with row-wise MODE
    df_num <- fill_tail_na_with_row_mode(df_num, tail_n = llm_tail)
    
    # Drop remaining NA rows (humans)
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
    
    # ---------- HUMAN CCT ----------
    hum <- cct_stats(if (length(human_idx)) df_num[human_idx, , drop = FALSE] else df_num[0, , drop = FALSE])
    
    hum_raw <- if (length(hum$comp)) hum$comp * sqrt(n_human) else numeric(0)
    
    q99_human_local <- NA_real_
    if (length(hum_raw) > 0) {
      q99_human_local <- as.numeric(quantile(hum_raw, probs = 0.99, na.rm = TRUE, type = 8))
      if (!is.finite(q99_human_local) || q99_human_local <= 0) q99_human_local <- max(hum_raw, na.rm = TRUE)
    }
    
    hum_q99 <- if (length(hum_raw)) pmin(hum_raw / q99_human_local, 1) else numeric(0)
    
    hum_mat  <- if (n_human > 0) df_num[human_idx, , drop = FALSE] else df_num[0, , drop = FALSE]
    hum_cons <- weighted_item_consensus(hum_mat, hum$comp)
    
    human_consensus_str <- if (length(hum_cons)) paste0(round(hum_cons, 6), collapse = ";") else ""
    
    # ---------- PREP: LLM TAIL INDEXING (stride by 10) ----------
    llm_rows_all   <- llm_idx
    llm_labels_all <- if (length(llm_rows_all)) labels[llm_rows_all] else character(0)
    
    # ---------- HUMAN ROW (keep) ----------
    empty_model_cols <- as.list(setNames(rep(NA_real_, length(llm_model_cols)), llm_model_cols))
    
    rows[[length(rows) + 1]] <- data.frame(
      domain = domain, country = cty, group = "human", llm_model = "",
      source_csv = csv_path,
      n_group = n_human,
      eig_ratio = hum$er, variance_explained = hum$ve,
      comp_raw_mean = if (length(hum$comp)) mean(hum$comp, na.rm = TRUE) else NA_real_,
      comp_q99_mean = if (length(hum_q99)) mean(hum_q99, na.rm = TRUE) else NA_real_,
      q99_local_anchor_human = q99_human_local,
      q99_local_anchor_llm   = NA_real_,
      llm_ids = "", llm_comp_raw_values = "", llm_comp_q99_values = "",
      llm_comp_raw_map = "", llm_comp_q99_map = "",
      human_consensus_raw_values = human_consensus_str,
      llm_consensus_raw_values   = "",
      pct_agreement_consensus    = NA_real_,
      stringsAsFactors = FALSE,
      empty_model_cols
    )
    
    # ---------- 10 LLM ROWS (each base model uses 6 prompts as respondents) ----------
    if (length(llm_rows_all) > 0) {
      for (mi in seq_along(llm_model_base)) {
        # positions within the LLM tail: mi, mi+10, mi+20, ..., mi+50
        pos <- mi + 10 * (0:5)
        pos <- pos[pos <= length(llm_rows_all)]
        if (!length(pos)) next
        
        m_rows   <- llm_rows_all[pos]
        m_labels <- llm_labels_all[pos]
        
        m_mat <- df_num[m_rows, , drop = FALSE]
        m_n   <- nrow(m_mat)
        
        m_stats <- cct_stats(m_mat)
        
        # local Q99 scaling for THIS MODEL (6 prompts)
        m_raw <- if (length(m_stats$comp)) m_stats$comp * sqrt(m_n) else numeric(0)
        
        q99_llm_local <- NA_real_
        if (length(m_raw) > 0) {
          q99_llm_local <- as.numeric(quantile(m_raw, probs = 0.99, na.rm = TRUE, type = 8))
          if (!is.finite(q99_llm_local) || q99_llm_local <= 0) q99_llm_local <- max(m_raw, na.rm = TRUE)
        }
        
        m_q99 <- if (length(m_raw)) pmin(m_raw / q99_llm_local, 1) else numeric(0)
        
        # consensus for this model
        m_cons <- weighted_item_consensus(m_mat, m_stats$comp)
        m_consensus_str <- if (length(m_cons)) paste0(round(m_cons, 6), collapse = ";") else ""
        
        # percent agreement vs human consensus
        pct_agreement_consensus <- if (length(hum_cons) && length(m_cons) && length(hum_cons) == length(m_cons)) {
          mean(round(hum_cons) == round(m_cons), na.rm = TRUE) * 100
        } else {
          NA_real_
        }
        
        # strings for this model (6)
        llm_ids_str <- if (length(m_labels)) paste0(m_labels, collapse = ";") else ""
        llm_raw_vals_str <- if (length(m_stats$comp)) paste0(round(m_stats$comp, 6), collapse = ";") else ""
        llm_q99_vals_str <- if (length(m_q99)) paste0(round(m_q99, 6), collapse = ";") else ""
        
        llm_raw_map_str <- if (length(m_labels) && length(m_stats$comp) == length(m_labels)) {
          paste0(m_labels, ":", round(m_stats$comp, 6), collapse = ";")
        } else ""
        
        llm_q99_map_str <- if (length(m_labels) && length(m_q99) == length(m_labels)) {
          paste0(m_labels, ":", round(m_q99, 6), collapse = ";")
        } else ""
        
        # wide model columns: set ONLY this model's column to mean(q99), others NA
        model_cols_vals <- as.list(setNames(rep(NA_real_, length(llm_model_cols)), llm_model_cols))
        model_cols_vals[[llm_model_cols[mi]]] <- if (length(m_q99)) mean(m_q99, na.rm = TRUE) else NA_real_
        
        rows[[length(rows) + 1]] <- data.frame(
          domain = domain, country = cty, group = "llm", llm_model = llm_model_base[mi],
          source_csv = csv_path,
          n_group = m_n,
          eig_ratio = m_stats$er, variance_explained = m_stats$ve,
          comp_raw_mean = if (length(m_stats$comp)) mean(m_stats$comp, na.rm = TRUE) else NA_real_,
          comp_q99_mean = if (length(m_q99)) mean(m_q99, na.rm = TRUE) else NA_real_,
          q99_local_anchor_human = q99_human_local,
          q99_local_anchor_llm   = q99_llm_local,
          llm_ids = llm_ids_str,
          llm_comp_raw_values = llm_raw_vals_str,
          llm_comp_q99_values = llm_q99_vals_str,
          llm_comp_raw_map = llm_raw_map_str,
          llm_comp_q99_map = llm_q99_map_str,
          human_consensus_raw_values = human_consensus_str,
          llm_consensus_raw_values   = m_consensus_str,
          pct_agreement_consensus    = pct_agreement_consensus,
          stringsAsFactors = FALSE,
          model_cols_vals
        )
      }
    }
  }
  
  if (length(rows) > 0) {
    out_df <- do.call(rbind, rows)
    out_df <- out_df[order(out_df$country, out_df$group, out_df$llm_model), ]
    utils::write.csv(out_df, out_csv, row.names = FALSE)
    message(sprintf("[DONE] Wrote %d rows to: %s", nrow(out_df), out_csv))
  } else {
    message(sprintf("[SKIP] No valid data found for domain: %s", domain))
  }
}