# ==============================================================================
# STEP 3: IDENTITY MATRIX
# ==============================================================================

#' Identity Matrix Test (Bartlett)
#'
#' @param data data.frame containing ONLY the significant variables
#'             (+ optional non-numeric grouping column).
#' @param file Output text file (default: "identity_matrix_results.txt").
#' @return List with chi2, df, p-value, interpretation.
#' @examples
#' set.seed(1)
#' example_data <- data.frame(
#'   L1    = rnorm(20, 10, 1),
#'   L2    = rnorm(20, 5, 0.5),
#'   Group = rep(c("A", "B"), each = 10)
#' )
#' old_wd <- setwd(tempdir())
#' identity_matrix_test(example_data, file = "identity_matrix_results.txt")
#' setwd(old_wd)
#' @export
identity_matrix_test <- function(data, file = "identity_matrix_results.txt") {

  if (!requireNamespace("psych", quietly = TRUE))
    stop("The 'psych' package is required.")

  data.numeric <- Filter(is.numeric, data)
  data.clean   <- stats::na.omit(data.numeric)
  cor_mat <- stats::cor(data.clean, use = "complete.obs")
  test_result  <- psych::cortest.bartlett(cor_mat, n = nrow(data.clean))

  interpretation <- if (is.na(test_result$p.value)) {
    "Bartlett's test could not be computed (invalid matrix)."
  } else if (test_result$p.value < 0.05) {
    paste("The correlation matrix is NOT an identity matrix (H0 rejected).",
          "Significant correlations present: multicollinearity detected.")
  } else {
    paste("The correlation matrix is an identity matrix (H0 not rejected).",
          "Independent variables. No multicollinearity.")
  }

  output <- paste0(
    "=== BARTLETT TEST ON CORRELATION MATRIX ===\n",
    "(Variables tested: significant variables from step 2)\n\n",
    "Chi2: ", ifelse(is.na(test_result$chisq), "Not computable",
                      round(test_result$chisq, 4)), "\n",
    "Degrees of freedom: ", test_result$df, "\n",
    "p-value: ", ifelse(is.na(test_result$p.value), "Not computable",
                         test_result$p.value), "\n\n",
    "Interpretation:\n", interpretation, "\n"
  )
  cat(output)
  write(output, file = file)
  message("Step 3 - Results exported to: ", file)
  invisible(list(chisq = test_result$chisq, df = test_result$df,
                 p_value = test_result$p.value, interpretation = interpretation))
}
