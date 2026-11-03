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
[MetricFishJ](https://github.com/owonicolvs3457).

## Installation

```r
# 1. Install the package from GitHub (dependencies are installed automatically)
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("owonicolvs3457/MetricFishR", upgrade = "never")

# 2. Load the package
library(MetricFishR)
```

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
group <- names(my_data)[ncol(my_data)]   # grouping column (last)
identity_matrix_test(my_data[, c(inf$significant, group)])
```

## Dependencies

`psych`, `ggcorrplot`, `ggplot2`, `FactoMineR`, `factoextra`, `RColorBrewer`,
`FSA`, `openxlsx`, as well as `tcltk`, `grDevices`, `stats` and `utils` (base R).
If `rlang` is installed, version `>= 1.1.7` is required.

## License

MIT © OWO Nicolas Roland

## Bugs & suggestions

https://github.com/owonicolvs3457/MetricFishR/issues
