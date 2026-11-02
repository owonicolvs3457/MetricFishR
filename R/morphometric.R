# ==============================================================================
# COMPLETE PIPELINE (without DFA)
# ==============================================================================

#' Morphometric Analysis Pipeline -- 7 steps (without DFA)
#'
#' Flow of variables:
#'   Step 2  -> significant variables (p < 0.05)
#'              used by steps 3 AND 4
#'   Step 4  -> kept variables (after correlation exclusion)
#'              used by step 6 (Wilks)
#'   Step 6  -> retained variables (F > GUI threshold)
#'              used by step 7 (PCA)
#'
#' @param data         complete data.frame (last column = BIN/grouping).
#' @param palette_name Name of the RColorBrewer color palette for the PCA
#'   (step 7; e.g. "Set1", "Set2", "Dark2", "Paired", ...). If `NULL`
#'   (default), a tcltk window offers the choice in an interactive session
#'   (see [pca_analysis()]).
#' @return `NULL`, invisibly. The pipeline is run for its
#'   side effects (files and plots exported at each step).
#' @examples
#' \dontrun{
#' # Chains the 7 steps, including two interactive windows (steps 4 and 6):
#' # requires an interactive session.
#' morphometric(my_data)
#' }
#' @export
morphometric <- function(data, palette_name = NULL) {

  cat("\n==================================================\n")
  cat("  MORPHOMETRIC PIPELINE - OWO Nicolas Roland (without DFA)\n")
  cat("==================================================\n\n")

  # -- STEP 1
  cat(">>> STEP 1: Descriptive statistics (Excel export only)\n")
  descriptive_stats(data)

  # -- STEP 2
  cat("\n>>> STEP 2: Inferential analysis\n")
  inf_res  <- inferential_stats(data)
  sig_vars <- inf_res$significant

  if (length(sig_vars) == 0) {
    message("No significant variable. Pipeline stopped after step 2.")
    return(invisible(NULL))
  }

  cat("\nSignificant variables retained:", paste(sig_vars, collapse = ", "), "\n")
  group_col    <- names(data)[ncol(data)]
  data.non.num <- data[, !sapply(data, is.numeric), drop = FALSE]

  data_sig <- cbind(data[, sig_vars, drop = FALSE], data.non.num)

  # -- STEP 3
  cat("\n>>> STEP 3: Identity matrix (significant variables from step 2)\n")
  identity_matrix_test(data_sig)

  # -- STEP 4
  cat("\n>>> STEP 4: Pearson correlation + interactive exclusion (significant variables from step 2)\n")
  corr_res  <- pearson_correlation(data_sig)
  kept_vars <- corr_res$kept_variables

  # -- STEP 5
  cat("\n>>> STEP 5: List of kept variables\n")
  if (length(kept_vars) > 0) {
    list_kept_variables(kept_vars)
  } else {
    message("Step 5 skipped: no variable kept.")
  }

  # -- STEP 6
  cat("\n>>> STEP 6: Wilks' lambda test\n")
  cat("    Basis: variables kept in step 4 (",
      paste(kept_vars, collapse = ", "), ")\n")
  wilks_res <- list(significant = c())
  if (length(kept_vars) > 0) {
    data_wilks <- data[, c(kept_vars, group_col), drop = FALSE]
    wilks_res  <- wilks_test(data_wilks)
  } else {
    message("Step 6 skipped: no variable available.")
  }

  vars_wilks <- wilks_res$significant

  # -- STEP 7: PCA
  cat("\n>>> STEP 7: PCA\n")
  if (length(vars_wilks) > 0) {
    cat("    Basis: variables retained by Wilks (",
        paste(vars_wilks, collapse = ", "), ")\n")
    pca_analysis(data[, c(vars_wilks, group_col), drop = FALSE],
                palette_name = palette_name)
  } else {
    message("Step 7 skipped: no variable retained by Wilks.")
  }

  cat("\n==================================================\n")
  cat("  PIPELINE COMPLETED (DFA removed)\n")
  cat("==================================================\n")

  invisible(NULL)
}
