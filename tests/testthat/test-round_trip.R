test_that("Writing and reading a panel works", {
  # Create a temporary excel workbook file -------------------------------------
  filepath_workbook <- tempfile(fileext = ".xlsx")
  # Save and write some panel sheet data ---------------------------------------
  ps_data_sent <- build_panel_sheet(
    panel_name = "Round-trip Test Panel",
    panel_description =
      paste0(
        "This is to create a round-trip test of the build_panel_sheet, ",
        "write_panel_sheet, and subsequent reading functions."
      ),
    n_samples = 2L,
    sample_groups = c("Positive", "Negative"),
    sample_matrices = "Serum",
    analytes = c("IgM", "IgG", "Pan-Ig"),
    targets = "SARS-CoV-2 Spike Protein Antigen",
    qualitative_outcomes = c("Positive", "Negative"),
    qualitative_comparators = "CDC Spike Antigen Assay",
    semiquantitative_outcomes = NA_character_,
    semiquantitative_comparators = NA_character_,
    quantitative_units = NA_character_,
    quantitative_comparators = NA_character_
  )
  write_panel_sheet(
    panel_sheet_data = ps_data_sent,
    filepath = filepath_workbook,
    method = "excel"
  )
  # Read that file -------------------------------------------------------------
  ps_data_returned <-
    read_panel(
      filepath = filepath_workbook
    )

  expect_equal(
    object = ps_data_returned,
    expected = ps_data_sent
  )
})

test_that("proper errors are thrown", {
  expect_error(
    object = read_panel("bad_filepath.doc"),
    regexp = ".*\\.xlsx.*"
  )
  expect_error(
    object =
      read_panel_data(
        filepath = "okay.xlsx",
        method = "garbage"
      ),
    regexp = ".*excel.*"
  )
  expect_error(
    object =
      read_panel_metadata(
        filepath = "okay.xlsx",
        method = "garbage"
      ),
    regexp = ".*excel.*"
  )
  expect_error(
    object =
      panel_sheet_csv_method("anything"),
    regexp = "^Reserved.*"
  )
})
