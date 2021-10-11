#' Write Evaluation Sheet
#'
#' Generate a template sample evaluation sheet to record ground truth for an
#' evaluation or series of evaluations.
#'
#' @param evaluation_sheet_data The evaluation data, which are the output of
#'   \code{dxr::build_evaluation_sheet()}.
#' @param filepath The filepath to write the evaluation sheet to.
#' @param method File format to output to. Currently, the only option is the
#'   default, \code{"excel"}.
#'
#' @return An evaluation sheet template file, written to disk.
#' @export
#'
write_evaluation_sheet <- function(evaluation_sheet_data, filepath,
                                   method = "excel") {
  workbook <-
    evaluation_sheet_excel_method(
      evaluation_sheet_data = evaluation_sheet_data
    )
  openxlsx::saveWorkbook(
    wb = workbook,
    file = filepath,
    overwrite = TRUE
  )
}

#' Evaluation Sheet Excel Method
#'
#' Build an excel workbook, using \code{openxlsx}.
#'
#' @param evaluation_sheet_data The evaluation data, which are the output of
#'   \code{dxr::build_evaluation_sheet()}.
#'
#' @return A workbook object from \code{openxlsx}.
#'
evaluation_sheet_excel_method <- function(evaluation_sheet_data) {
  metadata <-
    evaluation_sheet_data$evaluation_metadata %>%
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
  style_datetime <-
    openxlsx::createStyle(
      numFmt = "LONGDATE",
      locked = FALSE
    )

  # Create Workbook ------------------------------------------------------------
  workbook <-
    openxlsx::createWorkbook()

  # Build metadata sheet -------------------------------------------------------
  openxlsx::addWorksheet(
    wb = workbook,
    sheetName = "evaluation_metadata",
    tabColour = "yellow",
    header =
      c(
        # Left side header:
        "",
        # Center header:
        paste(evaluation_sheet_data$evaluation_metadata$value[[1]], "Metadata"),
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
    sheet = "evaluation_metadata",
    x = evaluation_sheet_data$evaluation_metadata,
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
    sheet = "evaluation_metadata",
    style = style_locked_gray,
    rows = seq_along(metadata) + 1,
    cols = 1,
    gridExpand = TRUE
  )
  # Second column, all data rows
  openxlsx::addStyle(
    wb = workbook,
    sheet = "evaluation_metadata",
    style = style_locked_gray,
    rows = seq_along(metadata) + 1,
    cols = 2,
    gridExpand = TRUE
  )
  # But make it possible to edit the description
  unprotect_cells <- c("evaluation_description", "evaluation_name")
  openxlsx::addStyle(
    wb = workbook,
    sheet = "evaluation_metadata",
    style = style_wrap_unlocked,
    rows = which(names(metadata) %in% unprotect_cells) + 1,
    cols = 2,
    gridExpand = TRUE
  )

  # First row, two columns
  openxlsx::addStyle(
    wb = workbook,
    sheet = "evaluation_metadata",
    style = style_header,
    rows = 1,
    cols = c(1, 2),
    gridExpand = TRUE
  )
  # Make things fit on a page nicely.
  openxlsx::setColWidths(
    wb = workbook,
    sheet = "evaluation_metadata",
    cols = c(1, 2),
    widths = c(28, 58)
  )
  # Lock it down.
  openxlsx::protectWorksheet(
    wb = workbook,
    sheet = "evaluation_metadata",
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

  # Build blind sheet ----------------------------------------------------------
  # For now, we'll store the unblinding data in the same excel file in a hidden
  # sheet.
  # TODO: Create the option to store blinding data in a different file.
  openxlsx::addWorksheet(
    wb = workbook,
    sheetName = "sample_blinding",
    tabColour = "red",
    header =
      c(
        # Left side header:
        "",
        # Center header:
        paste(
          evaluation_sheet_data$evaluation_metadata$value[[1]],
          "Sample Blinding"
        ),
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
    gridLines = FALSE,
    visible = FALSE
  )
  ## Write Data ----------------------------------------------------------------
  openxlsx::writeData(
    wb = workbook,
    sheet = "sample_blinding",
    x = evaluation_sheet_data$sample_blinding,
    startCol = 1,
    startRow = 1,
    colNames = TRUE,
    rowNames = FALSE,
    keepNA = TRUE,
    na.string = "None"
  )

  ## Apply Styles --------------------------------------------------------------
  # First two columns, all data rows
  openxlsx::addStyle(
    wb = workbook,
    sheet = "sample_blinding",
    style = style_locked_gray,
    rows =
      seq_along(evaluation_sheet_data$sample_blinding$evaluation_sample_id) + 1,
    cols = 1:2,
    gridExpand = TRUE
  )
  # Header
  openxlsx::addStyle(
    wb = workbook,
    sheet = "sample_blinding",
    style = style_header,
    rows = 1,
    cols = 1:2,
    gridExpand = TRUE
  )
  # Make things fit on a page nicely.
  openxlsx::setColWidths(
    wb = workbook,
    sheet = "sample_blinding",
    cols = c(1, 2),
    widths = c(28, 28)
  )
  openxlsx::protectWorksheet(
    wb = workbook,
    sheet = "sample_blinding",
    protect = TRUE,
    lockSelectingLockedCells = FALSE,
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
    sheetName = "evaluation_data",
    tabColour = "green",
    header =
      c(
        # Left side header:
        "",
        # Center header:
        paste(evaluation_sheet_data$evaluation_metadata$value[[1]], "Data"),
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
  ## Write data ----------------------------------------------------------------
  openxlsx::writeDataTable(
    wb = workbook,
    sheet = "evaluation_data",
    x = evaluation_sheet_data$evaluation_table
  )
  ## Apply styles --------------------------------------------------------------
  # Lock header row
  openxlsx::addStyle(
    wb = workbook,
    sheet = "evaluation_data",
    style = style_locked,
    rows = 1,
    cols = seq_along(colnames(evaluation_sheet_data$evaluation_table)),
    gridExpand = TRUE
  )
  # Unlock data cells
  data_cells <-
    tidyr::crossing(
      rows = seq_along(evaluation_sheet_data$evaluation_table$sample) + 1,
      cols = seq_along(colnames(evaluation_sheet_data$evaluation_table))
    )
  openxlsx::addStyle(
    wb = workbook,
    sheet = "evaluation_data",
    style = style_unlocked,
    rows = data_cells$rows,
    cols = data_cells$cols
  )
  # Format datetime column
  datetime_column <-
    which(
      colnames(evaluation_sheet_data$evaluation_table) == "datetime_observation"
    )
  openxlsx::addStyle(
    wb = workbook,
    sheet = "evaluation_data",
    style = style_datetime,
    rows = seq_along(evaluation_sheet_data$evaluation_table$sample) + 1,
    cols = datetime_column,
    gridExpand = TRUE
  )
  # Set column widths
  openxlsx::setColWidths(
    wb = workbook,
    sheet = "evaluation_data",
    cols = seq_along(colnames(evaluation_sheet_data$evaluation_table)),
    widths = "auto"
  )
  # Lock it down.
  openxlsx::protectWorksheet(
    wb = workbook,
    sheet = "evaluation_data",
    protect = TRUE,
    lockSelectingLockedCells = TRUE,
    lockSelectingUnlockedCells = FALSE,
    lockFormattingCells = TRUE,
    lockFormattingColumns = TRUE,
    lockFormattingRows = TRUE,
    lockInsertingColumns = TRUE,
    lockInsertingRows = TRUE,
    lockInsertingHyperlinks = TRUE,
    lockDeletingColumns = TRUE,
    lockDeletingRows = TRUE,
    lockSorting = FALSE,
    lockAutoFilter = FALSE,
    lockPivotTables = FALSE,
    lockObjects = TRUE,
    lockScenarios = TRUE
  )

  # Workbook protection and saving ---------------------------------------------
  openxlsx::protectWorkbook(
    wb = workbook,
    protect = TRUE
  )

  workbook
}

#' Evaluation Sheet CSV Method
#'
#' @param evaluation_sheet_data The evaluation data, which are the output of
#'   \code{dxr::build_evaluation_sheet()}.
#'
#' @return Nothing for now, just an error.
#'
evaluation_sheet_csv_method <- function(evaluation_sheet_data) {
  stop("Reserved. This function hasn't been written yet.")
}
