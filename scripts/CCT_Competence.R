library(devtools)
library(AnthroTools)

# ---- settings ----
base_dir    <- here("ARR_March", "Data")
output_dir <- here("ARR_March", "Output")

domains_list <- c("HWB", "POC", "POM", "POS", "EV", "RV", "EVN", "POST",
                  "SCTOM/I", "SCTOM/II", "SCTOM/III",
                  "SVNS/I", "SVNS/II", "SVNS/III",
                  "PIPP/I", "PIPP/II", "PCPR/I", "PCPR/II")
domains_list <- c("SCTOM/II")

countries <- c("Armenia", "Germany", "Greece", "Japan", "Netherlands",
               "Colombia", "Malaysia", "Mexico", "Peru", "United_States")

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

# ---- model list for stable output columns ----
base_models <- c(
  "GPT-OSS_120B","LLAMA3_1_70B","LLAMA3_70B","PHI3_INSTRUCT",
  "QWEN2_5VL_32B","QWEN2_5VL_72B","QWEN2_5VL_7B","QWEN3_32B","QWEN_7B","GPT_4O"
)

# ---- helpers ----
fmt_num <- function(x, digits = 6) {
  paste(format(round(x, digits), nsmall = digits, trim = TRUE), collapse = ";")
}
strip_suffix <- function(x) sub("_[^_]+$", "", x)  

# ---- output folder once ----
out_dir <- file.path(output_dir, "summaries", "Competence")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

for (domain in domains_list) {
  
  domain_safe <- sanitize_for_filename(domain)
  out_csv <- file.path(out_dir, sprintf("Competence_%s.csv", domain_safe))
  
  if (file.exists(out_csv)) file.remove(out_csv)
  
  first_write <- TRUE
  
  for (cty in countries) {
    
    cty_dir  <- file.path(base_dir, domain, cty)
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
    df_num <- fill_tail_na_with_row_mode(df_num, tail_n = llm_tail)
    
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
    
    llm_idx   <- if (n_llm > 0) seq.int(n_total - n_llm + 1, n_total) else integer(0)
    human_idx <- if (n_human > 0) seq.int(1, n_human) else integer(0)
    
    combined <- cct_stats(df_num)
    if (!length(combined$comp)) {
      message(sprintf("[SKIP] No competence vector for %s / %s", domain, cty))
      next
    }
    
    # 1. Raw competence calculation (L2 norm * sqrt(N))
    comp_raw <- combined$comp * sqrt(n_total)
    
    # 2. Local Q99 anchor (calculated specifically for this country's respondents)
    q99_local <- as.numeric(quantile(comp_raw, probs = 0.99, na.rm = TRUE, type = 8))
    if (!is.finite(q99_local) || q99_local <= 0) q99_local <- max(comp_raw, na.rm = TRUE)
    
    # 3. Apply Local Q99 scaling and cap at 1
    combined_q99 <- pmin(comp_raw / q99_local, 1)
    
    # Extract string formatted components
    comp_all_str   <- fmt_num(combined_q99)
    comp_human_str <- if (length(human_idx)) fmt_num(combined_q99[human_idx]) else ""
    comp_llm_str   <- if (length(llm_idx))   fmt_num(combined_q99[llm_idx])   else ""
    
    mean_comp_human <- if (length(human_idx)) mean(combined_q99[human_idx], na.rm = TRUE) else NA_real_
    mean_comp_llm   <- if (length(llm_idx))   mean(combined_q99[llm_idx],   na.rm = TRUE) else NA_real_
    
    mean_by_model <- setNames(rep(NA_real_, length(base_models)),
                              paste0("mean_", base_models))
    
    if (length(llm_idx)) {
      llm_df <- data.frame(
        label = labels[llm_idx],
        comp  = combined_q99[llm_idx],
        stringsAsFactors = FALSE
      )
      llm_df$base_model <- strip_suffix(llm_df$label)
      
      llm_mean <- aggregate(comp ~ base_model, llm_df, mean, na.rm = TRUE)
      
      for (i in seq_len(nrow(llm_mean))) {
        coln <- paste0("mean_", llm_mean$base_model[i])
        if (coln %in% names(mean_by_model)) mean_by_model[coln] <- llm_mean$comp[i]
      }
    }
    
    out_row <- cbind(
      data.frame(
        domain = domain,
        country = cty,
        n_human = n_human,
        n_llm = n_llm,
        mean_comp_human = mean_comp_human,
        mean_comp_llm = mean_comp_llm,
        competence_all = comp_all_str,
        competence_human = comp_human_str,
        competence_llm = comp_llm_str,
        stringsAsFactors = FALSE,
        check.names = FALSE
      ),
      as.data.frame(as.list(mean_by_model), check.names = FALSE)
    )
    
    # Write to CSV in real-time
    if (first_write) {
      write.csv(out_row, out_csv, row.names = FALSE, quote = TRUE)
      first_write <- FALSE
    } else {
      write.table(out_row, out_csv, sep = ",", row.names = FALSE,
                  col.names = FALSE, append = TRUE, quote = TRUE)
    }
  }
  
  message(sprintf("[DONE] Wrote domain summary: %s", out_csv))
}