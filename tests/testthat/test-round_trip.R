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

test_that("round-tripping nci_1 works", {
  # Create a temporary excel workbook file -------------------------------------
  filepath_workbook <- tempfile(fileext = ".xlsx")
  # Save and write some panel sheet data ---------------------------------------
  write_panel_sheet(
    panel_sheet_data = nci_1,
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
    expected = nci_1
  )
})

test_that("writing and reading an evaluation works", {
  # Create a temporary excel workbook file -------------------------------------
  filepath_workbook <- tempfile(fileext = ".xlsx")
  # Save and write some panel sheet data ---------------------------------------
  eval_data_sent <-
    build_evaluation_sheet(
      evaluation_name = "Example Evaluation",
      evaluation_description = NA_character_,
      developer = "ACME Test Corp.",
      assay = "Test Assay #1",
      lot_numbers = "20200101",
      panel_data =
        build_panel_sheet(
          panel_name = "Example Panel",
          panel_description = "An example panel.",
          n_samples = 5L,
          sample_groups = "Samples",
          sample_matrices = "Serum",
          analytes = c("IgM", "IgG", "Pan-Ig"),
          targets = "Spike",
          qualitative_outcomes = c("Positive", "Negative"),
          qualitative_comparators = "Authorized NAAT and CDC Assay",
          semiquantitative_outcomes = NA,
          semiquantitative_comparators = NA,
          quantitative_units = NA,
          quantitative_comparators = NA
        ),
      analytes = c("IgM", "IgG", "Pan-Ig"),
      targets = "Spike",
      qualitative_outcomes = c("Positive", "Negative"),
      semiquantitative_outcomes = NA_character_,
      quantitative_units = NA_character_,
      randomize = FALSE,
      blind = FALSE
    )
  # Write evaluation -----------------------------------------------------------
  write_evaluation_sheet(
    evaluation_sheet_data = eval_data_sent,
    filepath = filepath_workbook,
    method = "excel"
  )
  # Read that file -------------------------------------------------------------
  eval_data_returned <-
    read_evaluation(
      filepath = filepath_workbook
    )

  expect_equal(
    object = eval_data_returned,
    expected = eval_data_sent
  )
})
