write_panel_sheet <- function(filepath, panel_sheet_data, method = "excel") {
  filepath = "test.xlsx"
  panel_sheet_data = case_1


  workbook <-
    openxlsx::createWorkbook()

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
    orientation = "portrait"
  )
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
    orientation = "landscape"
  )
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
    na.string = "None",
    borders = "surrounding",
    borderStyle = "medium"
  )
  openxlsx::writeDataTable(
    wb = workbook,
    sheet = "panel_data",
    x = panel_sheet_data$panel_table
  )
  openxlsx::saveWorkbook(
    wb = workbook,
    file = filepath,
    overwrite = TRUE
  )

}

panel_sheet_excel_method <- function(filepath, panel_sheet_data) {

}

panel_sheet_csv_method <- function(filepath, panel_sheet_data) {

}
