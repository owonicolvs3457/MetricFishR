<div align="center">
  <img src="MetricFishR_logo.png" alt="MetricFishR logo" width="220"/>
</div>

# MetricFishR

> An integrated morphometric pipeline for fish diversity studies.

**MetricFishR** chains the usual statistical steps of a linear morphometric
study into a single seven-step pipeline: descriptive statistics, inferential
tests, Bartlett's identity test, Pearson correlation, Wilks' lambda test and
principal component analysis (PCA) — **without discriminant factor analysis
(DFA)**. Each step passes its selected variables to the next one and writes its
results to files, so the whole analysis can be archived and repeated.

Package developed by **OWO Nicolas Roland**. It is the statistical counterpart
of the ImageJ/Fiji plugin
[MetricFishJ](https://github.com/owonicolvs3457), which extracts the
measurements from images.

## Installation

```r
# 1. Install the package from GitHub (dependencies are installed automatically)
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("owonicolvs3457/MetricFishR", upgrade = "never")

# 2. Load the package
library(MetricFishR)
```

If R tells you that the package is "in use", restart R (Session > Restart R)
and run the installation again before loading the package.

Requires R >= 4.1.0.

## Input data

A single `data.frame` with:

- one row per specimen;
- numeric columns for the measurements (traits);
- the grouping variable (site, population, species, molecular unit) in the
  **last column**.

## The 7-step pipeline

| Step | Function                 | Description                                                         |
|:----:|--------------------------|---------------------------------------------------------------------|
| 1    | `descriptive_stats()`    | Descriptive statistics (Min, Max, Mean ± SD, CV, homogeneity) → Excel |
| 2    | `inferential_stats()`    | Parametric / non-parametric tests + post hoc → significant variables |
| 3    | `identity_matrix_test()` | Bartlett's test on the significant variables                         |
| 4    | `pearson_correlation()`  | Pearson correlation matrix + interactive exclusion (tcltk)           |
| 5    | `list_kept_variables()`  | Export of the kept variables                                         |
| 6    | `wilks_test()`           | Wilks' lambda test (adjustable F threshold) + ANOVA / Tukey HSD      |
| 7    | `pca_analysis()`         | PCA (FactoMineR / factoextra) + plot export (png / jpg / svg)        |

The variable flow follows the steps: `2 -> 3 & 4 -> 6 -> 7`.

### Test selection (step 2)

For each trait, a Shapiro-Wilk test decides between:

- **parametric branch**: Bartlett's test of variance equality, then a t-test or
  ANOVA with Tukey's post hoc test;
- **non-parametric branch**: Wilcoxon test, or Kruskal-Wallis test with Dunn's
  post hoc test (Bonferroni correction).

Traits that cannot be tested (too few distinct values, a single group) are
excluded and reported instead of stopping the analysis.

## Quick start

Run the whole pipeline in cascade on a complete `data.frame`
(last column = grouping variable):

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

The PCA palette can be given as an argument so that the last step runs without
a selection window:

```r
morphometric(my_data, palette_name = "Set1")
```

## Interactive choices

Two decisions are left to the analyst:

- **Step 4**: which of two strongly correlated traits to exclude (default
  threshold on the absolute Pearson correlation: 0.8);
- **Step 6**: the minimum F value a trait needs to be kept for the PCA.

In an interactive session these choices are made in Tcl/Tk windows.

## Outputs

Results are written to the working directory: Excel and text files for the
statistics, and PNG, JPG and SVG files for the plots (correlation matrix, PCA
biplot and variable circle).

## Dependencies

`psych`, `ggcorrplot`, `ggplot2`, `FactoMineR`, `factoextra`, `RColorBrewer`,
`FSA`, `openxlsx`, as well as `tcltk`, `grDevices`, `stats` and `utils` (base R).
If `rlang` is installed, version `>= 1.1.7` is required.

## Citation

If you use MetricFishR, please cite the package:

> OWO N.R. (2026). *MetricFishR: Integrated Morphometric Pipeline for Fish
> Diversity*. R package version 0.1.0.
> https://github.com/owonicolvs3457/MetricFishR

## License

MIT © OWO Nicolas Roland

## Bugs & suggestions

https://github.com/owonicolvs3457/MetricFishR/issues
