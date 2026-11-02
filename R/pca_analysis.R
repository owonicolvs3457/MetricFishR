# ==============================================================================
# STEP 7: PCA  ->  on the variables retained by Wilks
# ==============================================================================

# Qualitative RColorBrewer palettes available for the PCA
pca_palette_choices <- c("Set1", "Set2", "Set3", "Dark2", "Paired",
                         "Accent", "Pastel1", "Pastel2")

#' Interactive tcltk GUI to choose the PCA color palette
#'
#' Internal function used by [pca_analysis()] to select, via a
#' tcltk window, the color palette (RColorBrewer) used for the
#' PCA plots.
#'
#' @param choices Vector of the proposed palette names.
#' @param default Palette selected by default.
#' @return Name of the chosen palette (character string).
#' @keywords internal
gui_palette_select <- function(choices = pca_palette_choices, default = "Set1") {

  selected <- default

  tt <- tcltk::tktoplevel()
  tcltk::tkwm.title(tt, "Step 7 - PCA: Color palette")
  .rsp_geometry(tt, 360, 150)
  tcltk::tkwm.resizable(tt, FALSE, FALSE)

  tcltk::tkpack(tcltk::tklabel(tt,
                               text = "Choose the color palette for the PCA plots:",
                               font = "Arial 10 bold", foreground = "#1a3a5c",
                               wraplength = 340, pady = 10))

  var_palette <- tcltk::tclVar(default)
  cb <- tcltk::ttkcombobox(tt, values = choices, textvariable = var_palette,
                           state = "readonly", width = 20)
  tcltk::tkpack(cb, pady = 6)

  confirm <- function() {
    selected <<- tcltk::tclvalue(var_palette)
    tcltk::tkdestroy(tt)
  }
  tcltk::tkpack(tcltk::tkbutton(tt, text = "  Confirm  ", command = confirm,
                                background = "#1a3a5c", foreground = "white",
                                font = "Arial 11 bold", padx = 12), pady = 10)

  tcltk::tkwait.window(tt)
  selected
}

#' Builds a color palette for n groups from a qualitative
#' RColorBrewer palette, extending it if necessary.
#'
#' @param palette_name Name of the RColorBrewer palette (e.g. "Set1", "Dark2").
#' @param n_groups     Number of groups to color.
#' @return Vector of `n_groups` colors.
#' @keywords internal
build_pca_palette <- function(palette_name, n_groups) {

  max_colors <- RColorBrewer::brewer.pal.info[palette_name, "maxcolors"]
  n_base     <- max(3, min(n_groups, max_colors))
  base_cols  <- RColorBrewer::brewer.pal(n_base, palette_name)

  if (n_groups <= max_colors) {
    base_cols[seq_len(n_groups)]
  } else {
    grDevices::colorRampPalette(base_cols)(n_groups)
  }
}

#' Principal Component Analysis -- on the variables retained by Wilks
#'
#' @param data         data.frame (last column = grouping).
#' @param ncp          Number of components (default: 5).
#' @param file_eigen   Eigenvalues file (default: "pca_eigenvalues.txt").
#' @param file_var     Variable correlations file (default: "pca_variables.txt").
#' @param palette_name Name of the RColorBrewer color palette to use for
#'   the plots (e.g. "Set1", "Set2", "Dark2", "Paired", ...). If `NULL`
#'   (default) and the session is interactive, a tcltk window offers the
#'   choice among the available qualitative palettes; otherwise "Set1" is
#'   used.
#' @return FactoMineR PCA object.
#' @examples
#' \donttest{
#' # Writes several plots (.png/.jpg/.svg) in the current directory.
#' set.seed(1)
#' example_data <- data.frame(
#'   L1    = c(rnorm(10, 10, 1), rnorm(10, 14, 1)),
#'   L2    = c(rnorm(10, 5, 0.5), rnorm(10, 5.2, 0.5)),
#'   L3    = c(rnorm(10, 2, 0.3), rnorm(10, 2.5, 0.3)),
#'   Group = rep(c("A", "B"), each = 10)
#' )
#' old_wd <- setwd(tempdir())
#' pca_analysis(example_data, ncp = 2, palette_name = "Dark2")
#' setwd(old_wd)
#' }
#' @export
pca_analysis <- function(data,
                         ncp          = 5,
                         file_eigen   = "pca_eigenvalues.txt",
                         file_var     = "pca_variables.txt",
                         palette_name = NULL) {

  data.numeric <- Filter(is.numeric, data)
  group        <- as.factor(data[[ncol(data)]])
  n_groups     <- length(levels(group))

  if (is.null(palette_name)) {
    palette_name <- tryCatch({
      if (!interactive()) stop("non-interactive session")
      gui_palette_select(pca_palette_choices, default = "Set1")
    }, error = function(e) "Set1")
  }

  if (!palette_name %in% pca_palette_choices) {
    warning("Unknown palette '", palette_name, "', using 'Set1' instead.")
    palette_name <- "Set1"
  }

  message("Step 7 - Color palette used: ", palette_name)
  palette <- build_pca_palette(palette_name, n_groups)

  res.pca <- FactoMineR::PCA(data.numeric, ncp = ncp, graph = FALSE)

  vp <- factoextra::get_eigenvalue(res.pca)
  print(vp)
  utils::capture.output(vp, file = file_eigen)
  message("Step 7 - Eigenvalues exported to: ", file_eigen)

  res.var <- factoextra::get_pca_var(res.pca)
  utils::capture.output(res.var$cor, file = file_var)
  message("Step 7 - Variable correlations exported to: ", file_var)

  p_var <- factoextra::fviz_pca_var(res.pca, col.var = "contrib",
                                    gradient.cols = c("blue", "yellow", "green"), repel = TRUE)
  print(p_var)
  ggplot2::ggsave("pca_variables_circle.png", plot = p_var, width = 10, height = 8, dpi = 300)
  ggplot2::ggsave("pca_variables_circle.jpg", plot = p_var, width = 10, height = 8, dpi = 300)
  ggplot2::ggsave("pca_variables_circle.svg", plot = p_var, width = 10, height = 8)

  p_bi <- factoextra::fviz_pca_biplot(res.pca, axes = 1:2,
                                      geom.ind = "point", col.ind = group, habillage = group,
                                      repel = TRUE, col.var = "black",
                                      addEllipses = TRUE, ellipse.type = "convex", palette = palette)
  print(p_bi)
  ggplot2::ggsave("pca_biplot.png", plot = p_bi, width = 12, height = 8, dpi = 300)
  ggplot2::ggsave("pca_biplot.jpg", plot = p_bi, width = 12, height = 8, dpi = 300)
  ggplot2::ggsave("pca_biplot.svg", plot = p_bi, width = 12, height = 8)

  p_ind <- factoextra::fviz_pca_ind(res.pca, col.ind = group,
                                    repel = TRUE, geom.ind = "point", habillage = group,
                                    palette = palette, addEllipses = TRUE, ellipse.type = "convex")
  print(p_ind)
  ggplot2::ggsave("pca_individuals.png", plot = p_ind, width = 12, height = 8, dpi = 300)
  ggplot2::ggsave("pca_individuals.jpg", plot = p_ind, width = 12, height = 8, dpi = 300)
  ggplot2::ggsave("pca_individuals.svg", plot = p_ind, width = 12, height = 8)

  message("Step 7 - All plots exported (.png, .jpg, .svg)")

  res.desc <- FactoMineR::dimdesc(res.pca, axes = c(1, 2), proba = 0.05)
  cat("\n=== Variables correlated with Dim.1 ===\n"); print(res.desc$Dim.1)
  cat("\n=== Variables correlated with Dim.2 ===\n"); print(res.desc$Dim.2)

  invisible(res.pca)
}
