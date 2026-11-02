# ==============================================================================
# STEP 5: LIST OF KEPT VARIABLES
# ==============================================================================

#' List Kept Variables
#'
#' @param kept_vars Vector of kept variable names.
#' @param file      Output text file (default: "kept_variables.txt").
#' @return The `kept_vars` vector, invisibly.
#' @examples
#' old_wd <- setwd(tempdir())
#' list_kept_variables(c("L1", "L2"), file = "kept_variables.txt")
#' setwd(old_wd)
#' @export
list_kept_variables <- function(kept_vars, file = "kept_variables.txt") {
  cat("\n=== VARIABLES KEPT AFTER STEP 4 ===\n")
  for (i in seq_along(kept_vars)) cat(sprintf("  %d. %s\n", i, kept_vars[i]))
  cat("\n")
  writeLines(c("Kept variables (basis of the Wilks test):", kept_vars), con = file)
  message("Step 5 - List exported to: ", file)
  invisible(kept_vars)
}
