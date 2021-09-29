#' Title
#'
#' @param filepath
#' @param panel_sheet_data
#' @param method
#'
#' @return
#' @export
#'
#' @examples
write_panel_sheet <- function(filepath, panel_sheet_data, method = "excel") {
  workbook <- panel_sheet_excel_method(panel_sheet_data = panel_sheet_data)
  openxlsx::saveWorkbook(
    wb = workbook,
    file = filepath,
    overwrite = TRUE
  )
}

#' Title
#'
#' @param panel_sheet_data
#'
#' @return
#'
#' @examples
panel_sheet_excel_method <- function(panel_sheet_data) {
  metadata <-
    panel_sheet_data$panel_metadata %>%
    tidyr::pivot_wider() %>%
    as.list()

  # Set up styles --------------------------------------------------------------
  style_locked_gray <-
    openxlsx::createStyle(
      numFmt = "TEXT",
      fgFill = "#999999",
      halign = "left",
      valign = "top",
      locked = TRUE
    )
  style_header <-
    openxlsx::createStyle(
      numFmt = "TEXT",
      fgFill = "#999999",
      halign = "center",
      valign = "center",
      locked = TRUE,
      textDecoration = "bold"
    )
  style_wrap_unlocked <-
    openxlsx::createStyle(
      numFmt = "TEXT",
      wrapText = TRUE,
      fgFill = "#FAB57F",
      halign = "left",
      valign = "top",
      locked = FALSE
    )
  style_unlocked <-
    openxlsx::createStyle(
      locked = FALSE
    )
  style_locked <-
    openxlsx::createStyle(
      locked = TRUE
    )

  # Create Workbook ------------------------------------------------------------
  workbook <-
    openxlsx::createWorkbook()

  # Build metadata sheet -------------------------------------------------------
  openxlsx::addWorksheet(
    wb = workbook,
    sheetName = "panel_metadata",
    tabColour = "yellow",
    header =
      c(
        # Left side header:
        "",
        # Center header:
        paste(panel_sheet_data$panel_metadata$value[[1]], "Metadata"),
        # Right side header
        ""
      ),
    footer =
      c(
        # Left side footer:
        "",
        # Center footer:
        "Page &[Page] of &[Pages]",
        # Right side footer
        ""
      ),
    paperSize = 1, # Letter paper
    orientation = "portrait",
    gridLines = FALSE
  )
  ## Write data ----------------------------------------------------------------
  openxlsx::writeData(
    wb = workbook,
    sheet = "panel_metadata",
    x = panel_sheet_data$panel_metadata,
    startCol = 1,
    startRow = 1,
    colNames = TRUE,
    rowNames = FALSE,
    keepNA = TRUE,
    sep = "; ",
    na.string = "None"
  )
  ## Apply styles --------------------------------------------------------------
  # First column, all data rows
  openxlsx::addStyle(
    wb = workbook,
    sheet = "panel_metadata",
    style = style_locked_gray,
    rows = seq_along(metadata) + 1,
    cols = 1,
    gridExpand = TRUE
  )
  # Second column, all data rows
  openxlsx::addStyle(
    wb = workbook,
    sheet = "panel_metadata",
    style = style_locked_gray,
    rows = seq_along(metadata) + 1,
    cols = 2,
    gridExpand = TRUE
  )
  # But make it possible to edit the description
  unprotect_cells <- c("panel_description", "panel_name")
  openxlsx::addStyle(
    wb = workbook,
    sheet = "panel_metadata",
    style = style_wrap_unlocked,
    rows = which(names(metadata) %in% unprotect_cells) + 1,
    cols = 2,
    gridExpand = TRUE
  )

  # First row, two columns
  openxlsx::addStyle(
    wb = workbook,
    sheet = "panel_metadata",
    style = style_header,
    rows = 1,
    cols = c(1, 2),
    gridExpand = TRUE
  )
  # Make things fit on a page nicely.
  openxlsx::setColWidths(
    wb = workbook,
    sheet = "panel_metadata",
    cols = c(1, 2),
    widths = c(28, 58)
  )
  # Lock it down.
  openxlsx::protectWorksheet(
    wb = workbook,
    sheet = "panel_metadata",
    protect = TRUE,
    lockSelectingLockedCells = TRUE,
    lockSelectingUnlockedCells = FALSE,
    lockFormattingCells = TRUE,
    lockFormattingColumns = FALSE,
    lockFormattingRows = FALSE,
    lockInsertingColumns = TRUE,
    lockInsertingRows = TRUE,
    lockInsertingHyperlinks = FALSE,
    lockDeletingColumns = TRUE,
    lockDeletingRows = TRUE,
    lockSorting = TRUE,
    lockAutoFilter = TRUE,
    lockPivotTables = TRUE,
    lockObjects = TRUE,
    lockScenarios = TRUE
  )

  # Build data sheet -----------------------------------------------------------
  openxlsx::addWorksheet(
    wb = workbook,
    sheetName = "panel_data",
    tabColour = "green",
    header =
      c(
        # Left side header:
        "",
        # Center header:
        paste(panel_sheet_data$panel_metadata$value[[1]], "Data"),
        # Right side header
        ""
      ),
    footer =
      c(
        # Left side footer:
        "",
        # Center footer:
        "Page &[Page] of &[Pages]",
        # Right side footer
        ""
      ),
    paperSize = 1, # Letter paper
    orientation = "landscape",
    gridLines = FALSE
  )
  # Write data -----------------------------------------------------------------
  openxlsx::writeDataTable(
    wb = workbook,
    sheet = "panel_data",
    x = panel_sheet_data$panel_table
  )
  # Apply styles ---------------------------------------------------------------
  # Lock header row
  openxlsx::addStyle(
    wb = workbook,
    sheet = "panel_data",
    style = style_locked,
    rows = 1,
    cols = seq_along(colnames(panel_sheet_data$panel_table)),
    gridExpand = TRUE
  )
  # Unlock data cells
  data_cells <-
    tidyr::crossing(
      rows = seq_along(panel_sheet_data$panel_table$sample) + 1,
      cols = seq_along(colnames(panel_sheet_data$panel_table))
    )
  openxlsx::addStyle(
    wb = workbook,
    sheet = "panel_data",
    style = style_unlocked,
    rows = data_cells$rows,
    cols = data_cells$cols
  )
  openxlsx::setColWidths(
    wb = workbook,
    sheet = "panel_data",
    cols = seq_along(colnames(panel_sheet_data$panel_table)),
    widths = "auto"
  )
  # Lock it down.
  openxlsx::protectWorksheet(
    wb = workbook,
    sheet = "panel_data",
    protect = TRUE,
    lockSelectingLockedCells = TRUE,
    lockSelectingUnlockedCells = FALSE,
    lockFormattingCells = TRUE,
    lockFormattingColumns = FALSE,
    lockFormattingRows = FALSE,
    lockInsertingColumns = TRUE,
    lockInsertingRows = TRUE,
    lockInsertingHyperlinks = FALSE,
    lockDeletingColumns = TRUE,
    lockDeletingRows = TRUE,
    lockSorting = FALSE,
    lockAutoFilter = FALSE,
    lockPivotTables = FALSE,
    lockObjects = TRUE,
    lockScenarios = TRUE
  )

  # Workbook protection and saving ----------------------------------------------
  openxlsx::protectWorkbook(
    wb = workbook,
    protect = TRUE
  )
}

#' Title
#'
#' @param panel_sheet_data
#'
#' @return
#'
#' @examples
panel_sheet_csv_method <- function(panel_sheet_data) {
  stop("Reserved. This function hasn't been written yet.")
}
