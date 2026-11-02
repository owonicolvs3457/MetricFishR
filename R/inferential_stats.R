# ==============================================================================
# STEP 2: INFERENTIAL ANALYSIS
# ==============================================================================

#' Check Variable Validity for Inferential Tests
#'
#' Internal function: checks that a variable can be submitted to the
#' inferential tests of step 2 (sufficient sample size, at least two groups,
#' non-zero variance).
#'
#' @param var   Numeric vector.
#' @param group Grouping vector.
#' @return TRUE if valid, FALSE otherwise.
#' @keywords internal
is_valid_variable <- function(var, group) {
  df <- data.frame(var = var, group = group)
  df <- df[!is.na(df$var) & !is.na(df$group), ]
  if (nrow(df) < 3)                  return(FALSE)
  if (length(unique(df$group)) < 2)  return(FALSE)
  if (length(unique(df$var)) <= 1)   return(FALSE)
  if (length(unique(df$var)) < 3)    return(FALSE)
  return(TRUE)
}


#' Inferential Statistical Analysis with Variable Validation and Post Hoc
#'
#' @param data data.frame (last column = grouping).
#' @param file Output text file (default: "results_inferential.txt").
#' @return List of results + vector of significant variables.
#' @examples
#' set.seed(1)
#' example_data <- data.frame(
#'   L1    = c(rnorm(10, 10, 1), rnorm(10, 14, 1)),
#'   L2    = c(rnorm(10, 5, 0.5), rnorm(10, 5.2, 0.5)),
#'   Group = rep(c("A", "B"), each = 10)
#' )
#' old_wd <- setwd(tempdir())
#' res <- inferential_stats(example_data, file = "results_inferential.txt")
#' setwd(old_wd)
#' res$significant
#' @export
inferential_stats <- function(data, file = "results_inferential.txt") {

  numerics_variables <- names(data)[sapply(data, is.numeric)]
  groups_variables   <- names(data)[ncol(data)]

  valid_vars    <- c()
  excluded_vars <- c()
  for (var in numerics_variables) {
    if (is_valid_variable(data[[var]], data[[groups_variables]])) {
      valid_vars <- c(valid_vars, var)
    } else {
      excluded_vars <- c(excluded_vars, var)
    }
  }

  if (length(excluded_vars) > 0) {
    cat("\nVariables excluded (tests not applicable):\n")
    print(excluded_vars)
  }
  numerics_variables <- valid_vars

  if (length(numerics_variables) == 0) {
    message("Step 2 - No valid variable for the inferential tests.")
    return(invisible(list(results = list(), significant = c())))
  }

  perform_nonparametric_tests <- function(variable, group, results) {
    if (length(unique(group)) == 2) {
      wt <- stats::wilcox.test(variable ~ group)
      results$Non_Parametric_Test <- list(
        Test_Type   = "Wilcoxon",
        Interpretation = ifelse(wt$p.value < 0.05,
                                "Significant difference between the two groups.",
                                "No significant difference."),
        p_value = wt$p.value)
      results$Post_Hoc <- "Not applicable (only two groups)"
    } else {
      kt <- stats::kruskal.test(variable ~ group)
      results$Non_Parametric_Test <- list(
        Test_Type   = "Kruskal-Wallis",
        Interpretation = ifelse(kt$p.value < 0.05,
                                "Significant differences between the groups.",
                                "No significant differences."),
        p_value = kt$p.value)
      if (kt$p.value < 0.05) {
        results$Post_Hoc <- FSA::dunnTest(variable ~ group, method = "bonferroni")$res
      } else {
        results$Post_Hoc <- "No significant difference, post hoc not performed."
      }
    }
    return(results)
  }

  analyze_variable <- function(variable_name, data, groups_variables) {
    cat("\n--- Analysis for:", variable_name, "---\n")
    variable <- data[[variable_name]]
    group    <- as.factor(data[[groups_variables]])
    results  <- list()

    st <- stats::shapiro.test(variable)
    results$Normality_Test <- list(
      Interpretation = ifelse(st$p.value > 0.05,
                              "Normal distribution.",
                              "Non-normal distribution."),
      p_value = st$p.value)
    cat("Shapiro-Wilk: p =", st$p.value, "\n")

    if (st$p.value > 0.05) {
      bt <- stats::bartlett.test(variable ~ group)
      results$Equality_of_Variances_Test <- list(
        Interpretation = ifelse(bt$p.value > 0.05,
                                "Homogeneous variances.",
                                "Non-homogeneous variances."),
        p_value = bt$p.value)
      cat("Bartlett: p =", bt$p.value, "\n")

      if (bt$p.value > 0.05) {
        if (length(unique(group)) == 2) {
          tt <- stats::t.test(variable ~ group)
          results$Parametric_Test <- list(
            Test_Type   = "t-test",
            Interpretation = ifelse(tt$p.value < 0.05,
                                    "Significant difference.",
                                    "No significant difference."),
            p_value = tt$p.value)
          results$Post_Hoc <- "Not applicable (only two groups)"
        } else {
          at <- stats::aov(variable ~ group)
          ap <- summary(at)[[1]]$`Pr(>F)`[1]
          results$Parametric_Test <- list(
            Test_Type   = "ANOVA",
            Interpretation = ifelse(ap < 0.05,
                                    "Significant differences.",
                                    "No significant differences."),
            p_value            = ap,
            Full_Results = summary(at))
          results$Post_Hoc <- if (ap < 0.05) stats::TukeyHSD(at)
          else "Post hoc not performed (p >= 0.05)."
        }
      } else {
        results <- perform_nonparametric_tests(variable, group, results)
      }
    } else {
      results <- perform_nonparametric_tests(variable, group, results)
    }
    return(results)
  }

  results_list          <- list()
  significant_variables <- c()

  for (var in numerics_variables) {
    results_list[[var]] <- analyze_variable(var, data, groups_variables)
    pval <- results_list[[var]]$Parametric_Test$p_value
    if (is.null(pval)) pval <- results_list[[var]]$Non_Parametric_Test$p_value
    if (!is.null(pval) && !is.na(pval) && pval < 0.05)
      significant_variables <- c(significant_variables, var)
  }

  utils::capture.output({
    cat("\n=== INFERENTIAL RESULTS ===\n")
    if (length(excluded_vars) > 0)
      cat("\nVariables excluded:", paste(excluded_vars, collapse = ", "), "\n")
    for (var in names(results_list)) {
      cat("\n--- Variable:", var, "---\n")
      ti <- results_list[[var]]
      if (!is.null(ti$Normality_Test))
        cat("Shapiro-Wilk: p =", ti$Normality_Test$p_value, "|",
            ti$Normality_Test$Interpretation, "\n")
      if (!is.null(ti$Equality_of_Variances_Test))
        cat("Bartlett: p =", ti$Equality_of_Variances_Test$p_value, "|",
            ti$Equality_of_Variances_Test$Interpretation, "\n")
      if (!is.null(ti$Parametric_Test))
        cat(ti$Parametric_Test$Test_Type, ": p =",
            ti$Parametric_Test$p_value, "|", ti$Parametric_Test$Interpretation, "\n")
      if (!is.null(ti$Non_Parametric_Test))
        cat(ti$Non_Parametric_Test$Test_Type, ": p =",
            ti$Non_Parametric_Test$p_value, "|", ti$Non_Parametric_Test$Interpretation, "\n")
      if (!is.null(ti$Post_Hoc)) { cat("\n--- Post Hoc ---\n"); print(ti$Post_Hoc) }
    }
  }, file = file)
  message("Step 2 - Results exported to: ", file)

  if (length(significant_variables) > 0) {
    data.non.numeric <- data[, !sapply(data, is.numeric), drop = FALSE]
    data_sig <- cbind(data[, significant_variables, drop = FALSE], data.non.numeric)
    utils::write.table(data_sig, "data_significant_variables.txt",
                sep = "\t", row.names = FALSE, quote = FALSE)
    message("Step 2 - Significant variables exported to: data_significant_variables.txt")
  } else {
    message("Step 2 - No significant variable found.")
  }

  invisible(list(results = results_list, significant = significant_variables))
}
