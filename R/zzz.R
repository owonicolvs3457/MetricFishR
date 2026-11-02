.onAttach <- function(libname, pkgname) {
  if (requireNamespace("rlang", quietly = TRUE) &&
      utils::packageVersion("rlang") < "1.1.7") {
    packageStartupMessage(
      "The installed 'rlang' package is too old for MetricFishR (>= 1.1.7 required). ",
      "Update it with install.packages(\"rlang\")."
    )
  }
}
