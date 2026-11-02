# ==============================================================================
# STEP 1: DESCRIPTIVE STATISTICS  ->  Excel export ONLY
# ==============================================================================

#' Descriptive Statistics -- Excel export only
#'
#' Columns: Variable | Min | Max | Mean±SD | CV | Homogeneity
#' The ± symbol is written as direct Unicode in the Excel cells.
#' No text file is generated.
#'
#' @param data      data.frame containing the data.
#' @param digits    Number of decimals (default: 2).
#' @param file_xlsx Output Excel file (default: "result_descriptive.xlsx").
#' @return data.frame of descriptive statistics.
#' @examples
#' example_data <- data.frame(
#'   L1    = c(10, 12, 11, 13, 9),
#'   L2    = c(5, 5.5, 6, 5.2, 4.8),
#'   Group = c("A", "A", "B", "B", "B")
#' )
#' old_wd <- setwd(tempdir())
#' descriptive_stats(example_data, file_xlsx = "result_descriptive.xlsx")
#' setwd(old_wd)
#' @export
descriptive_stats <- function(data,
                              digits    = 2,
                              file_xlsx = "result_descriptive.xlsx") {

  data.numeric <- Filter(is.numeric, data)
  if (ncol(data.numeric) == 0) stop("No numeric column found.")

  colnames_numeric <- names(data.numeric)
  n <- ncol(data.numeric)

  assess_homogeneity <- function(cv) {
    if (is.na(cv))         return("NA")
    else if (cv < 0.02)    return("Very homogeneous")
    else if (cv <= 0.25)   return("Homogeneous")
    else                   return("Heterogeneous")
  }

  Variable    <- character(n)
  Min         <- numeric(n)
  Max         <- numeric(n)
  Mean_SD     <- character(n)
  CV          <- numeric(n)
  Homogeneity <- character(n)

  for (i in seq_len(n)) {
    col <- data.numeric[[i]]
    m   <- round(mean(col, na.rm = TRUE), digits)
    s   <- round(stats::sd(col, na.rm = TRUE), digits)
    mn  <- round(min(col,  na.rm = TRUE), digits)
    mx  <- round(max(col,  na.rm = TRUE), digits)
    cv  <- ifelse(m != 0, round(s / m, digits), NA)

    Variable[i]    <- colnames_numeric[i]
    Min[i]         <- mn
    Max[i]         <- mx
    Mean_SD[i]     <- paste0(m, " \u00b1 ", s)
    CV[i]          <- ifelse(is.na(cv), NA, cv)
    Homogeneity[i] <- assess_homogeneity(cv)
  }

  excel_df <- data.frame(
    Variable    = Variable,
    Min         = Min,
    Max         = Max,
    "Mean\u00b1SD" = Mean_SD,
    CV          = CV,
    Homogeneity = Homogeneity,
    check.names = FALSE
  )

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Descriptive Stats")

  header_style <- openxlsx::createStyle(
    fontName       = "Arial",
    fontSize       = 11,
    textDecoration = "bold",
    fgFill         = "#1a3a5c",
    fontColour     = "#FFFFFF",
    halign         = "CENTER",
    border         = "TopBottomLeftRight",
    borderColour   = "#FFFFFF"
  )
  data_style <- openxlsx::createStyle(
    fontName     = "Arial",
    fontSize     = 10,
    halign       = "CENTER",
    border       = "TopBottomLeftRight",
    borderColour = "#CCCCCC"
  )
  band_style <- openxlsx::createStyle(
    fontName     = "Arial",
    fontSize     = 10,
    halign       = "CENTER",
    fgFill       = "#EEF3F8",
    border       = "TopBottomLeftRight",
    borderColour = "#CCCCCC"
  )

  openxlsx::writeData(wb, "Descriptive Stats", excel_df, startRow = 1, startCol = 1)

  openxlsx::addStyle(wb, "Descriptive Stats", header_style,
                     rows = 1, cols = 1:ncol(excel_df), gridExpand = TRUE)
  for (r in 2:(nrow(excel_df) + 1)) {
    sty <- if ((r %% 2) == 0) data_style else band_style
    openxlsx::addStyle(wb, "Descriptive Stats", sty,
                       rows = r, cols = 1:ncol(excel_df), gridExpand = TRUE)
  }

  openxlsx::setColWidths(wb, "Descriptive Stats",
                         cols   = 1:ncol(excel_df),
                         widths = c(22, 10, 10, 18, 10, 16))

  openxlsx::saveWorkbook(wb, file = file_xlsx, overwrite = TRUE)
  message("Step 1 - Descriptive statistics exported to: ", file_xlsx)
  print(excel_df)
  invisible(excel_df)
}
