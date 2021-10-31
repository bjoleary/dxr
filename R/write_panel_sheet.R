#' Write Panel Sheet
#'
#' Generate a template sample panel sheet to record ground truth for an
#' evaluation or series of evaluations.
#'
#' @param panel_sheet_data The panel data, which are the output of
#'   \code{dxr::build_panel_sheet()}.
#' @param filepath The filepath to write the panel sheet to.
#' @param method File format to output to. Currently, the only option is the
#'   default, \code{"excel"}.
#'
#' @return An panel sheet template file, written to disk.
#' @export
#'
write_panel_sheet <- function(panel_sheet_data, filepath, method = "excel") {
  workbook <- panel_sheet_excel_method(panel_sheet_data = panel_sheet_data)
  openxlsx::saveWorkbook(
    wb = workbook,
    file = filepath,
    overwrite = TRUE
  )
}

#' Panel Sheet Excel Method
#'
#' Build an excel workbook, using \code{openxlsx}.
#'
#' @param panel_sheet_data The panel data, which are the output of
#'   \code{dxr::build_panel_sheet()}.
#'
#' @return A workbook object from \code{openxlsx}.
#'
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
  ## Write metadata ------------------------------------------------------------
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
  ## Write data ----------------------------------------------------------------
  openxlsx::writeDataTable(
    wb = workbook,
    sheet = "panel_data",
    x = panel_sheet_data$panel_table
  )
  ## Apply styles --------------------------------------------------------------
  ### Lock header row ----------------------------------------------------------
  openxlsx::addStyle(
    wb = workbook,
    sheet = "panel_data",
    style = style_locked,
    rows = 1,
    cols = seq_along(colnames(panel_sheet_data$panel_table)),
    gridExpand = TRUE
  )
  ### Unlock data cells---------------------------------------------------------
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
  ## Lock down worksheet -------------------------------------------------------
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
    lockInsertingHyperlinks = TRUE,
    lockDeletingColumns = TRUE,
    lockDeletingRows = TRUE,
    lockSorting = FALSE,
    lockAutoFilter = FALSE,
    lockPivotTables = FALSE,
    lockObjects = TRUE,
    lockScenarios = TRUE
  )

  # Input validation -----------------------------------------------------------
  ## Set up the data -----------------------------------------------------------
  # Not. Elegant. Don't judge, okay?
  # Make a vector for each input to validate
  analyte_validation <-
    metadata$analytes %>%
    unlist()
  target_validation <-
    metadata$targets %>%
    unlist()
  group_validation <-
    metadata$sample_groups %>%
    unlist()
  matrix_validation  <-
    metadata$sample_matrices %>%
    unlist()
  qual_truth_validation <-
    metadata$qualitative_outcomes %>%
    unlist()
  qual_comparator_validation <-
    metadata$qualitative_comparators %>%
    unlist()
  semi_truth_validation <-
    metadata$semiquantitative_outcomes %>%
    unlist()
  semi_comparator_validation <-
    metadata$semiquantitative_comparators %>%
    unlist()
  quant_units_validation <-
    metadata$quantitative_units %>%
    unlist()
  quant_comparator_validation <-
    metadata$quantitative_comparators %>%
    unlist()

  # Find the length of the longest vector
  longest <-
    max(
      length(analyte_validation),
      length(target_validation),
      length(group_validation),
      length(matrix_validation),
      length(qual_truth_validation),
      length(qual_comparator_validation),
      length(semi_truth_validation),
      length(semi_comparator_validation),
      length(quant_units_validation),
      length(quant_comparator_validation)
    )

  # Bind a lengthened version of each of those vectors as a column in the
  # new input_validation table
  input_validation <-
    dplyr::bind_cols(
      analyte_validation = lengthen(analyte_validation, longest),
      target_validation = lengthen(target_validation, longest),
      group_validation = lengthen(group_validation, longest),
      matrix_validation = lengthen(matrix_validation, longest),
      qual_truth_validation = lengthen(qual_truth_validation, longest),
      qual_comparator_validation =
        lengthen(qual_comparator_validation, longest),
      semi_truth_validation = lengthen(semi_truth_validation, longest),
      semi_comparator_validation =
        lengthen(semi_comparator_validation, longest),
      quant_units_validation = lengthen(quant_units_validation, longest),
      quant_comparator_validation =
        lengthen(quant_comparator_validation, longest)
    )

  ## Write the table to a new sheet --------------------------------------------
  openxlsx::addWorksheet(
    wb = workbook,
    sheetName = "input_validation",
    tabColour = "red",
    header =
      c(
        # Left side header:
        "",
        # Center header:
        paste(panel_sheet_data$panel_metadata$value[[1]], "Input Validation"),
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
    gridLines = FALSE,
    visible = FALSE
  )
  openxlsx::writeData(
    wb = workbook,
    sheet = "input_validation",
    x = input_validation,
    startCol = 1,
    startRow = 1,
    colNames = TRUE,
    rowNames = FALSE,
    keepNA = FALSE,
    na.string = ""
  )
  ## Apply input validation ----------------------------------------------------
  # TODO: No doubt this could be a map function instead.
  ### Analyte ------------------------------------------------------------------
  suppressWarnings(
    openxlsx::dataValidation(
      wb = workbook,
      sheet = "panel_data",
      cols = which(colnames(panel_sheet_data$panel_table) == "analyte"),
      rows = seq_along(panel_sheet_data$panel_table$sample) + 1,
      type = "list",
      operator = "between", # ignored
      value =
        validation_string(
          input_validation = input_validation,
          column_vector = analyte_validation
        ),
      allowBlank = TRUE,
      showInputMsg = TRUE,
      showErrorMsg = TRUE
    )
  )

  ### Target -------------------------------------------------------------------
  suppressWarnings(
    openxlsx::dataValidation(
      wb = workbook,
      sheet = "panel_data",
      cols = which(colnames(panel_sheet_data$panel_table) == "target"),
      rows = seq_along(panel_sheet_data$panel_table$sample) + 1,
      type = "list",
      operator = "between", # ignored
      value =
        validation_string(
          input_validation = input_validation,
          column_vector = target_validation
        ),
      allowBlank = TRUE,
      showInputMsg = TRUE,
      showErrorMsg = TRUE
    )
  )

  ### Group --------------------------------------------------------------------
  suppressWarnings(
    openxlsx::dataValidation(
      wb = workbook,
      sheet = "panel_data",
      cols = which(colnames(panel_sheet_data$panel_table) == "group"),
      rows = seq_along(panel_sheet_data$panel_table$sample) + 1,
      type = "list",
      operator = "between", # ignored
      value =
        validation_string(
          input_validation = input_validation,
          column_vector = group_validation
        ),
      allowBlank = TRUE,
      showInputMsg = TRUE,
      showErrorMsg = TRUE
    )
  )

  ### Matrix -------------------------------------------------------------------
  suppressWarnings(
    openxlsx::dataValidation(
      wb = workbook,
      sheet = "panel_data",
      cols = which(colnames(panel_sheet_data$panel_table) == "matrix"),
      rows = seq_along(panel_sheet_data$panel_table$sample) + 1,
      type = "list",
      operator = "between", # ignored
      value =
        validation_string(
          input_validation = input_validation,
          column_vector = matrix_validation
        ),
      allowBlank = TRUE,
      showInputMsg = TRUE,
      showErrorMsg = TRUE
    )
  )

  ### Qualitative Truth --------------------------------------------------------
  suppressWarnings(
    openxlsx::dataValidation(
      wb = workbook,
      sheet = "panel_data",
      cols =
        which(colnames(panel_sheet_data$panel_table) == "qualitative_truth"),
      rows = seq_along(panel_sheet_data$panel_table$sample) + 1,
      type = "list",
      operator = "between", # ignored
      value =
        validation_string(
          input_validation = input_validation,
          column_vector = qual_truth_validation
        ),
      allowBlank = TRUE,
      showInputMsg = TRUE,
      showErrorMsg = TRUE
    )
  )

  ### Qualitative Comparator ---------------------------------------------------
  suppressWarnings(
    openxlsx::dataValidation(
      wb = workbook,
      sheet = "panel_data",
      cols =
        which(
          colnames(panel_sheet_data$panel_table) == "qualitative_comparator"
        ),
      rows = seq_along(panel_sheet_data$panel_table$sample) + 1,
      type = "list",
      operator = "between", # ignored
      value =
        validation_string(
          input_validation = input_validation,
          column_vector = qual_comparator_validation
        ),
      allowBlank = TRUE,
      showInputMsg = TRUE,
      showErrorMsg = TRUE
    )
  )

  ### Semi Quantitative Truth and Comparator -----------------------------------
  if ("semiquantitative_truth" %in% colnames(panel_sheet_data$panel_table)) {
    suppressWarnings(
      openxlsx::dataValidation(
        wb = workbook,
        sheet = "panel_data",
        cols =
          which(
            colnames(panel_sheet_data$panel_table) == "semiquantitative_truth"
          ),
        rows = seq_along(panel_sheet_data$panel_table$sample) + 1,
        type = "list",
        operator = "between", # ignored
        value =
          validation_string(
            input_validation = input_validation,
            column_vector = semi_truth_validation
          ),
        allowBlank = TRUE,
        showInputMsg = TRUE,
        showErrorMsg = TRUE
      )
    )
    suppressWarnings(
      openxlsx::dataValidation(
        wb = workbook,
        sheet = "panel_data",
        cols =
          which(
            colnames(panel_sheet_data$panel_table) ==
              "semiquantitative_comparator"
          ),
        rows = seq_along(panel_sheet_data$panel_table$sample) + 1,
        type = "list",
        operator = "between", # ignored
        value =
          validation_string(
            input_validation = input_validation,
            column_vector = semi_comparator_validation
          ),
        allowBlank = TRUE,
        showInputMsg = TRUE,
        showErrorMsg = TRUE
      )
    )
  }

  ### Quantitative Truth and Comparator ----------------------------------------
  if ("quantitative_units" %in% colnames(panel_sheet_data$panel_table)) {
    suppressWarnings(
      openxlsx::dataValidation(
        wb = workbook,
        sheet = "panel_data",
        cols =
          which(
            colnames(panel_sheet_data$panel_table) == "quantitative_units"
          ),
        rows = seq_along(panel_sheet_data$panel_table$sample) + 1,
        type = "list",
        operator = "between", # ignored
        value =
          validation_string(
            input_validation = input_validation,
            column_vector = quant_units_validation
          ),
        allowBlank = TRUE,
        showInputMsg = TRUE,
        showErrorMsg = TRUE
      )
    )
    suppressWarnings(
      openxlsx::dataValidation(
        wb = workbook,
        sheet = "panel_data",
        cols =
          which(
            colnames(panel_sheet_data$panel_table) ==
              "quantitative_comparator"
          ),
        rows = seq_along(panel_sheet_data$panel_table$sample) + 1,
        type = "list",
        operator = "between", # ignored
        value =
          validation_string(
            input_validation = input_validation,
            column_vector = quant_comparator_validation
          ),
        allowBlank = TRUE,
        showInputMsg = TRUE,
        showErrorMsg = TRUE
      )
    )
  }
  ## Apply Styles --------------------------------------------------------------
  openxlsx::addStyle(
    wb = workbook,
    sheet = "input_validation",
    style = style_locked_gray,
    rows = c(1:longest) + 1,
    cols = seq_along(colnames(input_validation)),
    gridExpand = TRUE
  )
  # Header
  openxlsx::addStyle(
    wb = workbook,
    sheet = "input_validation",
    style = style_header,
    rows = 1,
    cols = seq_along(colnames(input_validation)),
    gridExpand = TRUE
  )
  openxlsx::setColWidths(
    wb = workbook,
    sheet = "input_validation",
    cols = seq_along(colnames(input_validation)),
    widths = "auto"
  )
  openxlsx::protectWorksheet(
    wb = workbook,
    sheet = "input_validation",
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
  # Workbook protection and saving ---------------------------------------------
  openxlsx::protectWorkbook(
    wb = workbook,
    protect = TRUE
  )

  workbook
}

#' Panel Sheet CSV Method
#'
#' @param panel_sheet_data The panel data, which are the output of
#'   \code{dxr::build_panel_sheet()}.
#'
#' @return Nothing for now, just an error.
#'
panel_sheet_csv_method <- function(panel_sheet_data) {
  stop("Reserved. This function hasn't been written yet.")
}
