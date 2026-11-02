# ==============================================================================
# STEP 4: PEARSON CORRELATION + TCLTK GUI
# ==============================================================================

#' Interactive tcltk GUI to exclude strongly-correlated variables
#'
#' Internal function used by [pearson_correlation()] to select,
#' via a tcltk window, the variables to exclude among the strongly
#' correlated pairs.
#'
#' @param vars       Vector of variable names.
#' @param cor_matrix    Pearson correlation matrix.
#' @param corr_threshold Strong correlation threshold (default: 0.8).
#' @return Vector of the variables selected for exclusion.
#' @keywords internal
gui_corr_exclude <- function(vars, cor_matrix, corr_threshold = 0.8) {

  strong_pairs        <- list()
  vars_in_pairs <- c()
  for (i in 1:(length(vars) - 1)) {
    for (j in (i + 1):length(vars)) {
      r <- cor_matrix[i, j]
      if (abs(r) > corr_threshold) {
        strong_pairs[[length(strong_pairs) + 1]] <- list(v1 = vars[i], v2 = vars[j], r = round(r, 3))
        vars_in_pairs <- unique(c(vars_in_pairs, vars[i], vars[j]))
      }
    }
  }

  tt <- tcltk::tktoplevel()
  tcltk::tkwm.title(tt, "Strong correlations - Significant variables (step 2)")
  .rsp_geometry(tt, 720, 580)
  tcltk::tkwm.resizable(tt, TRUE, TRUE)

  tcltk::tkpack(tcltk::tklabel(tt,
                               text = paste0("Strongly correlated significant variables (|r| > ", corr_threshold,
                                             ")  |  Select those to EXCLUDE"),
                               font = "Arial 12 bold", foreground = "#1a3a5c", wraplength = 680, pady = 8))

  frm_info <- tcltk::tkframe(tt, relief = "groove", borderwidth = 2)
  tcltk::tkpack(frm_info, fill = "x", padx = 14, pady = 4)
  tcltk::tkpack(tcltk::tklabel(frm_info, text = "Strongly correlated pairs:",
                               font = "Arial 10 bold", foreground = "#444444", anchor = "w"),
                fill = "x", padx = 6, pady = 2)

  sb_info <- tcltk::tkscrollbar(frm_info, orient = "vertical")
  lb_info <- tcltk::tklistbox(frm_info, selectmode = "browse",
                              yscrollcommand = function(...) tcltk::tkset(sb_info, ...),
                              font = "Courier 10", height = 9, background = "#f0f4f8", foreground = "#1a1a1a")
  tcltk::tkconfigure(sb_info, command = function(...) tcltk::tkyview(lb_info, ...))
  tcltk::tkpack(sb_info, side = "right", fill = "y")
  tcltk::tkpack(lb_info, side = "left", fill = "both", expand = TRUE, padx = 4, pady = 4)

  if (length(strong_pairs) == 0) {
    tcltk::tkinsert(lb_info, "end",
                    paste0("  No pair exceeds the threshold of ", corr_threshold))
  } else {
    tcltk::tkinsert(lb_info, "end",
                    sprintf("  %-20s  %-20s  %s", "Variable 1", "Variable 2", "r"))
    tcltk::tkinsert(lb_info, "end",
                    paste0("  ", paste(rep("-", 52), collapse = "")))
    for (p in strong_pairs)
      tcltk::tkinsert(lb_info, "end",
                      sprintf("  %-20s  %-20s  %+.3f", p$v1, p$v2, p$r))
  }

  tcltk::tkpack(tcltk::tklabel(tt,
                               text = "Select the variables to EXCLUDE (Ctrl+click = multiple selection):",
                               font = "Arial 10 bold", foreground = "#8b0000", anchor = "w", padx = 14, pady = 4),
                fill = "x")

  frm_sel <- tcltk::tkframe(tt)
  tcltk::tkpack(frm_sel, fill = "both", expand = TRUE, padx = 14, pady = 2)
  sb_sel <- tcltk::tkscrollbar(frm_sel, orient = "vertical")
  lb_sel <- tcltk::tklistbox(frm_sel, selectmode = "multiple",
                             yscrollcommand = function(...) tcltk::tkset(sb_sel, ...),
                             font = "Arial 11", height = 9, background = "#fff8f0", foreground = "#1a1a1a")
  tcltk::tkconfigure(sb_sel, command = function(...) tcltk::tkyview(lb_sel, ...))
  tcltk::tkpack(sb_sel, side = "right", fill = "y")
  tcltk::tkpack(lb_sel, side = "left", fill = "both", expand = TRUE)

  display_vars <- if (length(vars_in_pairs) > 0) vars_in_pairs else vars
  for (v in display_vars) tcltk::tkinsert(lb_sel, "end", v)

  tcltk::tkpack(tcltk::tklabel(tt,
                               text = "If nothing is selected: no variable is excluded.",
                               font = "Arial 9 italic", foreground = "#666666", pady = 2))

  selected <- character(0)
  confirm  <- function() {
    idx      <- as.integer(tcltk::tkcurselection(lb_sel))
    selected <<- if (length(idx) == 0) character(0) else display_vars[idx + 1]
    tcltk::tkdestroy(tt)
  }
  tcltk::tkpack(tcltk::tkbutton(tt,
                                text = "  Confirm exclusion  ", command = confirm,
                                background = "#1a3a5c", foreground = "white",
                                font = "Arial 11 bold", padx = 12), pady = 10)

  tcltk::tkwait.window(tt)
  return(selected)
}


#' Pearson Correlation Matrix -- on the significant variables of step 2
#'
#' @param data       data.frame containing ONLY the significant variables.
#' @param file_txt   Correlations text file (default: "correlation_pearson.txt").
#' @param file_img   Base image file (default: "correlation_matrix.jpg").
#' @param corr_threshold Strong correlation threshold (default: 0.8).
#' @return List: correlation matrix + vector of kept variables.
#' @examples
#' \dontrun{
#' # Opens an interactive tcltk window: requires an interactive session.
#' set.seed(1)
#' example_data <- data.frame(
#'   L1    = rnorm(20, 10, 1),
#'   L2    = rnorm(20, 5, 0.5),
#'   L3    = rnorm(20, 2, 0.3),
#'   Group = rep(c("A", "B"), each = 10)
#' )
#' pearson_correlation(example_data)
#' }
#' @export
pearson_correlation <- function(data,
                                file_txt   = "correlation_pearson.txt",
                                file_img   = "correlation_matrix.jpg",
                                corr_threshold = 0.8) {

  data.numeric <- Filter(is.numeric, data)
  vars         <- names(data.numeric)
  cor_matrix      <- stats::cor(data.numeric, use = "complete.obs")

  interpretations <- c(
    "=== PEARSON CORRELATION MATRIX ===",
    "(Variables: significant from step 2)\n",
    "Guide: |r| < 0.30 = Weak | 0.30-0.80 = Moderate | > 0.80 = Strong\n"
  )
  for (i in 1:(length(vars) - 1)) {
    for (j in (i + 1):length(vars)) {
      r     <- cor_matrix[i, j]
      force <- if (abs(r) < 0.30) "Weak" else if (abs(r) < 0.80) "Moderate" else "Strong"
      dir   <- if (r > 0) "positive" else "negative"
      interpretations <- c(interpretations,
                           sprintf("%s & %s: %s %s correlation (r = %.3f)", vars[i], vars[j], force, dir, r))
    }
  }
  writeLines(interpretations, con = file_txt)
  message("Step 4 - Correlations exported to: ", file_txt)

  plot_corr <- ggcorrplot::ggcorrplot(cor_matrix, method = "circle", type = "lower",
                                      lab = TRUE, lab_size = 3.5, colors = c("red", "white", "blue"))
  ggplot2::ggsave(file_img,                         plot = plot_corr, width = 16, height = 12, dpi = 600)
  ggplot2::ggsave(sub("\\.jpg$", ".png", file_img), plot = plot_corr, width = 16, height = 12, dpi = 600)
  ggplot2::ggsave(sub("\\.jpg$", ".svg", file_img), plot = plot_corr, width = 16, height = 12, units = "in")
  message("Step 4 - Plots exported (.jpg, .png, .svg)")

  exclude_cols <- tryCatch({
    gui_corr_exclude(vars, cor_matrix, corr_threshold)
  }, error = function(e) {
    cat("Graphical interface not available.\n")
    cat("Pairs (|r| >", corr_threshold, "):\n")
    mat_abs <- abs(cor_matrix); diag(mat_abs) <- 0
    strong  <- which(mat_abs > corr_threshold, arr.ind = TRUE)
    strong  <- strong[strong[, 1] < strong[, 2], , drop = FALSE]
    if (nrow(strong) > 0)
      for (k in seq_len(nrow(strong)))
        cat(sprintf("  %-18s & %-18s: r = %+.3f\n",
                    vars[strong[k, 1]], vars[strong[k, 2]],
                    cor_matrix[strong[k, 1], strong[k, 2]]))
    cat("Enter the variables to exclude, separated by commas (empty = none): ")
    input <- trimws(readLines(con = stdin(), n = 1))
    if (nchar(input) == 0) character(0) else trimws(strsplit(input, ",")[[1]])
  })

  keep_vars <- if (length(exclude_cols) > 0) {
    cat("Variables excluded:", paste(exclude_cols, collapse = ", "), "\n")
    vars[!vars %in% exclude_cols]
  } else {
    cat("No variable excluded.\n")
    vars
  }

  message("\n=== VARIABLES KEPT (-> step 6 Wilks) ===")
  for (i in seq_along(keep_vars)) message(sprintf("  %d. %s", i, keep_vars[i]))

  invisible(list(corr_matrix = cor_matrix, kept_variables = keep_vars))
}
