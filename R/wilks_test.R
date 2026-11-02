# ==============================================================================
# STEP 6: WILKS' LAMBDA TEST
# ==============================================================================

#' Wilks Lambda Test with Interactive F-threshold GUI
#'
#' @param data data.frame (numeric columns = step 4 variables, last = grouping).
#' @param file Output text file (default: "Test_Wilks_lambda-results.txt").
#' @return List: manova, significant (variables with F > threshold), f_threshold.
#' @examples
#' \dontrun{
#' # Opens a window to select the F threshold: requires an interactive session.
#' set.seed(1)
#' example_data <- data.frame(
#'   L1    = c(rnorm(10, 10, 1), rnorm(10, 14, 1)),
#'   L2    = c(rnorm(10, 5, 0.5), rnorm(10, 5.2, 0.5)),
#'   Group = rep(c("A", "B"), each = 10)
#' )
#' wilks_test(example_data)
#' }
#' @export
wilks_test <- function(data, file = "Test_Wilks_lambda-results.txt") {

  data.numeric     <- Filter(is.numeric, data)
  data.non.num     <- data[, !sapply(data, is.numeric), drop = FALSE]
  Cluster_variable <- names(data)[ncol(data)]
  df               <- data.frame(Cluster = as.factor(data[[Cluster_variable]]), data.numeric)
  m                <- as.matrix(data.numeric)

  n_obs           <- nrow(stats::na.omit(m))
  n_vars          <- ncol(m)
  manova_possible <- (n_vars < n_obs) && (n_obs > length(levels(df$Cluster)))

  manova_result <- NULL
  if (manova_possible) {
    manova_result <- stats::manova(m ~ Cluster, data = df)
  } else {
    warning(paste0("MANOVA not feasible: ", n_vars, " variables for ", n_obs,
                   " observations. Only univariate ANOVAs will be performed."))
  }

  proposed_thresholds <- c("5", "10", "15", "20", "25", "Other (free entry)")
  choice <- tryCatch({
    utils::select.list(choices  = proposed_thresholds,
                       title    = "F threshold to retain a variable (F > threshold):",
                       graphics = TRUE)
  }, error = function(e) {
    cat("Graphical interface not available.\n")
    cat("Choose the F threshold:", paste(proposed_thresholds, collapse = " | "), "\n> ")
    readLines(con = stdin(), n = 1)
  })

  if (choice == "Other (free entry)") {
    cat("Enter the F threshold value: ")
    f_threshold <- as.numeric(readLines(con = stdin(), n = 1))
  } else {
    f_threshold <- suppressWarnings(as.numeric(choice))
  }
  if (is.na(f_threshold) || f_threshold <= 0) {
    warning("Invalid threshold. Default value: 10.")
    f_threshold <- 10
  }
  cat("\nF threshold retained:", f_threshold, "\n")

  significant_vars <- c()
  f_values         <- c()

  utils::capture.output({
    cat("=== ANOVA results by variable ===\n")
    cat("(Variables tested: kept in step 4)\n\n")
    if (!is.null(manova_result)) print(stats::summary.aov(manova_result))
    else cat("MANOVA not performed.\n")

    cat("\n=== Wilks' test ===\n")
    if (!is.null(manova_result)) print(summary(manova_result, test = "Wilks"))
    else cat("Not computed (MANOVA impossible).\n")

    cat(sprintf("\n=== Univariate ANOVA + Tukey HSD (F threshold > %g) ===\n", f_threshold))
    for (var in colnames(data.numeric)) {
      cat(paste("\n--- Variable:", var, "---\n"))
      aov_result <- stats::aov(data.numeric[[var]] ~ df$Cluster)
      print(summary(aov_result))

      f_val <- summary(aov_result)[[1]][["F value"]][1]
      p_val <- summary(aov_result)[[1]][["Pr(>F)"]][1]

      if (!is.na(f_val) && f_val > f_threshold) {
        significant_vars <- c(significant_vars, var)
        f_values         <- c(f_values, f_val)
        cat(sprintf("  --> RETAINED (F = %.2f > threshold %.0f)\n", f_val, f_threshold))
      } else {
        cat(sprintf("  --> Not retained (F = %s)\n",
                    ifelse(is.na(f_val), "NA", round(f_val, 2))))
      }

      if (length(unique(df$Cluster)) > 2) print(stats::TukeyHSD(aov_result))
      else cat("Tukey test not performed (insufficient groups)\n")
    }

    cat("\n=== VARIABLES RETAINED FOR PCA ===\n")
    if (length(significant_vars) > 0) {
      for (k in seq_along(significant_vars))
        cat(sprintf("  %d. %s  (F = %.2f)\n", k, significant_vars[k], f_values[k]))
    } else {
      cat("  No variable exceeds the F threshold =", f_threshold, "\n")
    }
  }, file = file)

  message("Step 6 - Results exported to: ", file)
  message("Step 6 - F threshold retained: ", f_threshold)
  writeLines(significant_vars, "wilks_significant_variables.txt")

  if (length(significant_vars) > 0) {
    data_sig <- cbind(data[, significant_vars, drop = FALSE], data.non.num)
    utils::write.table(data_sig, "wilks_significant_data.txt",
                sep = "\t", row.names = FALSE, quote = FALSE)
    message("Step 6 - Retained variables exported to: wilks_significant_data.txt")
    message("Step 6 - These variables will be used for the PCA.")
  } else {
    message("Step 6 - No variable exceeds the F threshold > ", f_threshold, ".")
  }

  invisible(list(manova = manova_result, significant = significant_vars,
                 f_threshold = f_threshold))
}
