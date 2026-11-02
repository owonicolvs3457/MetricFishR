<div align="center">
  <img src="man/figures/logo.png" alt="MetricFishR logo" width="180"/>
</div>

# MetricFishR

Integrated morphometric pipeline for fish diversity: descriptive
statistics, inferential analysis, Bartlett's identity test, Pearson
correlation, Wilks' lambda test and principal component analysis
(PCA) — **without discriminant factor analysis (DFA)**.

Package developed by **OWO Nicolas Roland**, an R package adaptation of the
`morphometric.R` pipeline and the statistical counterpart of the ImageJ/Fiji plugin
[RMetricFishJ](https://github.com/owonicolvs3457).

## Installation

```r
# 1. Automatically install missing dependencies
deps <- c(
  "factoextra", "FactoMineR", "FSA", "ggcorrplot", "ggplot2", "openxlsx",
  "psych", "RColorBrewer"
)
miss <- setdiff(deps, rownames(installed.packages()))
if (length(miss)) install.packages(miss, dependencies = TRUE)

# 2. Install the package
install.packages("C:/path/to/RMetricFish_0.1.0.tar.gz", repos = NULL, type = "source")

# 3. Load the package (opens the interface)
library(MetricFishR)
```

Alternative: put `install_packages.R` (or `Install_packages.bat`) next to the `.tar.gz` file and run it: it installs missing dependencies (CRAN/Bioconductor) and then the package.

## The 7-step pipeline

| Step  | Function                   | Description                                                              |
|:-----:|----------------------------|---------------------------------------------------------------------------|
| 1     | `descriptive_stats()`      | Descriptive statistics (Min, Max, Mean±SD, CV, Homogeneity) → Excel      |
| 2     | `inferential_stats()`      | Non-parametric/parametric tests + post-hoc → significant variables        |
| 3     | `identity_matrix_test()`   | Bartlett test on the significant variables                               |
| 4     | `pearson_correlation()`    | Pearson correlation matrix + interactive exclusion (tcltk)                |
| 5     | `list_kept_variables()`    | Export of the kept variables                                             |
| 6     | `wilks_test()`             | Wilks' lambda test (adjustable F threshold) + ANOVA/TukeyHSD              |
| 7     | `pca_analysis()`           | PCA (FactoMineR/factoextra) + plot export (png/jpg/svg)                   |

The variable flow follows the steps: `2 -> 3 & 4 -> 6 -> 7`.

The `morphometric()` function runs the whole pipeline in cascade on a
complete `data.frame` (last column = grouping/BIN):

```r
library(MetricFishR)

morphometric(my_data)
```

Each step can also be called independently:

```r
descriptive_stats(my_data)
inf <- inferential_stats(my_data)
identity_matrix_test(my_data[, c(inf$significant, "Group")])
```

## Dependencies

`psych`, `ggcorrplot`, `ggplot2`, `FactoMineR`, `factoextra`, `RColorBrewer`,
`FSA`, `tcltk`, `ade4`, `MASS`, `dplyr`, `missForest`, `openxlsx`, as well as
`stats` and `utils` (base R). Requires `rlang >= 1.1.7`.

## License

MIT © OWO Nicolas Roland

## Bugs & suggestions

https://github.com/owonicolvs3457/MetricFishR/issues
