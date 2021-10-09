test_that("evaluations and panels must have compatable analytes", {
  expect_error(
    object =
      build_evaluation_sheet(
        evaluation_name = "Test Evaluation",
        evaluation_description = "A test...",
        developer = "Test Developer",
        assay = "The assay Name",
        lot_numbers = "20200101",
        panel_data =
          build_panel_sheet(
            panel_name = "Just a test",
            panel_description = "A test case.",
            n_samples = 5L,
            sample_groups = "Samples",
            sample_matrices = "Sputum",
            analytes = "SARS-CoV-2 Antigen",
            targets = "RBD",
            qualitative_outcomes = c("Positive", "Negative"),
            qualitative_comparators = "Authorized NAAT",
            semiquantitative_outcomes = NA,
            semiquantitative_comparators = NA,
            quantitative_units = NA,
            quantitative_comparators = NA
          ),
        panel_data_filepath = NA,
        analytes = "Garbage",
        targets = "RBD",
        qualitative_outcomes = c("Positive", "Negative"),
        semiquantitative_outcomes = NA_character_,
        quantitative_units = NA_character_
      ),
    regexp = "^At least one of the analytes.*Garbage.*"
  )
  expect_error(
    object =
      build_evaluation_sheet(
        evaluation_name = "Test Evaluation",
        evaluation_description = "A test...",
        developer = "Test Developer",
        assay = "The assay Name",
        lot_numbers = "20200101",
        panel_data =
          build_panel_sheet(
            panel_name = "Just a test",
            panel_description = "A test case.",
            n_samples = 5L,
            sample_groups = "Samples",
            sample_matrices = "Serum",
            analytes = c("IgM", "IgG"),
            targets = "Spike",
            qualitative_outcomes = c("Positive", "Negative"),
            qualitative_comparators = "Authorized NAAT and CDC Assay",
            semiquantitative_outcomes = NA,
            semiquantitative_comparators = NA,
            quantitative_units = NA,
            quantitative_comparators = NA
          ),
        panel_data_filepath = NA,
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = "RBD",
        qualitative_outcomes = c("Positive", "Negative"),
        semiquantitative_outcomes = NA_character_,
        quantitative_units = NA_character_
      ),
    regexp = "^At least one of the analytes.*"
  )
})


test_that("evaluations and panels should have compatible qual outcomes", {
  expect_warning(
    object =
      build_evaluation_sheet(
        evaluation_name = "Test Evaluation",
        evaluation_description = "A test...",
        developer = "Test Developer",
        assay = "The assay Name",
        lot_numbers = "20200101",
        panel_data =
          build_panel_sheet(
            panel_name = "Just a test",
            panel_description = "A test case.",
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
        panel_data_filepath = NA,
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = "RBD",
        qualitative_outcomes = c("Positive", "Equivocal", "Negative"),
        semiquantitative_outcomes = NA_character_,
        quantitative_units = NA_character_
      ),
    regexp = "^At least one of the qualitative outcomes.*Equivocal.*Equivocal"
  )
})

test_that("input checking works", {
  expect_type(
    # Working example -- no error
    object =
      build_evaluation_sheet(
        evaluation_name = "Test Evaluation",
        evaluation_description = "A test...",
        developer = "Test Developer",
        assay = "The assay Name",
        lot_numbers = "20200101",
        panel_data =
          build_panel_sheet(
            panel_name = "Just a test",
            panel_description = "A test case.",
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
        panel_data_filepath = NA,
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = "RBD",
        qualitative_outcomes = c("Positive", "Negative"),
        semiquantitative_outcomes = NA_character_,
        quantitative_units = NA_character_
      ),
    type = "list"
  )

  bad_char_input <-
    list(
      1L, # Not a character
      c("Name 1", "Name 2"), # Not length 1
      NA_character_, # NA
      NULL # Null
    )
  for (bad_input in bad_char_input) {
    # Evaluation Name
    expect_error(
      object =
        build_evaluation_sheet(
          evaluation_name = bad_input,
          evaluation_description = "A test...",
          developer = "Test Developer",
          assay = "The assay Name",
          lot_numbers = "20200101",
          panel_data =
            build_panel_sheet(
              panel_name = "Just a test",
              panel_description = "A test case.",
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
          panel_data_filepath = NA,
          analytes = c("IgM", "IgG", "Pan-Ig"),
          targets = "RBD",
          qualitative_outcomes = c("Positive", "Negative"),
          semiquantitative_outcomes = NA_character_,
          quantitative_units = NA_character_
        ),
      regexp = "\\(evaluation_name\\).* is not TRUE"
    )
  }

  bad_input_na_ok <-
    list(
      1L, # Not a character
      c("Name 1", "Name 2") # Not length 1
    )

  for (bad_input in bad_input_na_ok) {
    # Evaluation Description
    expect_error(
      object =
        build_evaluation_sheet(
          evaluation_name = "A test",
          evaluation_description = bad_input,
          developer = "Developer",
          assay = "The assay Name",
          lot_numbers = "20200101",
          panel_data =
            build_panel_sheet(
              panel_name = "Just a test",
              panel_description = "A test case.",
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
          panel_data_filepath = NA,
          analytes = c("IgM", "IgG", "Pan-Ig"),
          targets = "RBD",
          qualitative_outcomes = c("Positive", "Negative"),
          semiquantitative_outcomes = NA_character_,
          quantitative_units = NA_character_
        ),
      regexp = "\\(evaluation_description\\).* is not TRUE"
    )

    # Developer
    expect_error(
      object =
        build_evaluation_sheet(
          evaluation_name = "A test",
          evaluation_description = "A test...",
          developer = bad_input,
          assay = "The assay Name",
          lot_numbers = "20200101",
          panel_data =
            build_panel_sheet(
              panel_name = "Just a test",
              panel_description = "A test case.",
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
          panel_data_filepath = NA,
          analytes = c("IgM", "IgG", "Pan-Ig"),
          targets = "RBD",
          qualitative_outcomes = c("Positive", "Negative"),
          semiquantitative_outcomes = NA_character_,
          quantitative_units = NA_character_
        ),
      regexp = "\\(developer\\).* is not TRUE"
    )

    # Assay
    expect_error(
      object =
        build_evaluation_sheet(
          evaluation_name = "A test",
          evaluation_description = "A test...",
          developer = "Developer",
          assay = bad_input,
          lot_numbers = "20200101",
          panel_data =
            build_panel_sheet(
              panel_name = "Just a test",
              panel_description = "A test case.",
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
          panel_data_filepath = NA,
          analytes = c("IgM", "IgG", "Pan-Ig"),
          targets = "RBD",
          qualitative_outcomes = c("Positive", "Negative"),
          semiquantitative_outcomes = NA_character_,
          quantitative_units = NA_character_
        ),
      regexp = "\\(assay\\).* is not TRUE"
    )
  # Quantitative Units
  expect_error(
    object =
      build_evaluation_sheet(
        evaluation_name = "A test",
        evaluation_description = "A test...",
        developer = "Developer",
        assay = "Assay",
        lot_numbers = "20200101",
        panel_data =
          build_panel_sheet(
            panel_name = "Just a test",
            panel_description = "A test case.",
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
        panel_data_filepath = NA,
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = "RBD",
        qualitative_outcomes = c("Positive", "Negative"),
        semiquantitative_outcomes = NA_character_,
        quantitative_units = bad_input
      ),
    regexp = "\\(quantitative_units\\).* is not TRUE"
  )
  }

  bad_vector <- c(1:14)
  # Lot numbers
  expect_error(
    object =
      build_evaluation_sheet(
        evaluation_name = "A test",
        evaluation_description = "A test...",
        developer = "Developer",
        assay = "Assay",
        lot_numbers = bad_vector,
        panel_data =
          build_panel_sheet(
            panel_name = "Just a test",
            panel_description = "A test case.",
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
        panel_data_filepath = NA,
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = "RBD",
        qualitative_outcomes = c("Positive", "Negative"),
        semiquantitative_outcomes = NA_character_,
        quantitative_units = NA_character_
      ),
    regexp = "\\(lot_numbers.* is not TRUE"
  )
  # Qualitative outcomes
  expect_error(
    object =
      build_evaluation_sheet(
        evaluation_name = "A test",
        evaluation_description = "A test...",
        developer = "Developer",
        assay = "Assay",
        lot_numbers = "1234567",
        panel_data =
          build_panel_sheet(
            panel_name = "Just a test",
            panel_description = "A test case.",
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
        panel_data_filepath = NA,
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = "RBD",
        qualitative_outcomes = bad_vector,
        semiquantitative_outcomes = NA_character_,
        quantitative_units = NA_character_
      ),
    regexp = "\\(qualitative_outcomes.* is not TRUE"
  )
  # Semi-quantitative outcomes
  expect_error(
    object =
      build_evaluation_sheet(
        evaluation_name = "A test",
        evaluation_description = "A test...",
        developer = "Developer",
        assay = "Assay",
        lot_numbers = "1234567",
        panel_data =
          build_panel_sheet(
            panel_name = "Just a test",
            panel_description = "A test case.",
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
        panel_data_filepath = NA,
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = "RBD",
        qualitative_outcomes = c("Positive", "Negative"),
        semiquantitative_outcomes = bad_vector,
        quantitative_units = NA_character_
      ),
    regexp = "\\(semiquantitative_outcomes.* is not TRUE"
  )
  # Blind is not logical
  expect_error(
    object =
      build_evaluation_sheet(
        evaluation_name = "A test",
        evaluation_description = "A test...",
        developer = "Developer",
        assay = "Assay",
        lot_numbers = "1234567",
        panel_data =
          build_panel_sheet(
            panel_name = "Just a test",
            panel_description = "A test case.",
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
        panel_data_filepath = NA,
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = "RBD",
        qualitative_outcomes = c("Positive", "Negative"),
        semiquantitative_outcomes = NA_character_,
        quantitative_units = NA_character_,
        blind = "TRUE"
      ),
    regexp = "\\(blind\\) is not TRUE"
  )

  # Randomize is not logical
  expect_error(
    object =
      build_evaluation_sheet(
        evaluation_name = "A test",
        evaluation_description = "A test...",
        developer = "Developer",
        assay = "Assay",
        lot_numbers = "1234567",
        panel_data =
          build_panel_sheet(
            panel_name = "Just a test",
            panel_description = "A test case.",
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
        panel_data_filepath = NA,
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = "RBD",
        qualitative_outcomes = c("Positive", "Negative"),
        semiquantitative_outcomes = NA_character_,
        quantitative_units = NA_character_,
        randomize = "TRUE"
      ),
    regexp = "\\(randomize\\) is not TRUE"
  )
})

test_that("building the evaluation sheet works", {
  # First, let's build the expected output
  good_output <-
    list(
      evaluation_metadata =
        structure(
          list(
            name =
              c("evaluation_name", "evaluation_description", "developer",
                "assay", "lot_numbers", "analytes", "targets",
                "qualitative_outcomes", "semiquantitative_outcomes",
                "quantitative_units"),
            value =
              list("Test Evaluation", "A test...", "Test Developer",
                   "The assay Name", "20200101", c("IgM", "IgG", "Pan-Ig"),
                   "RBD", c("Positive", "Negative"), NA_character_,
                   NA_character_)),
          class = c("tbl_df", "tbl", "data.frame"),
          row.names = c(NA, -10L)),
      sample_blinding =
        structure(
          list(
            evaluation_sample_id =
              c("just_a_test_1", "just_a_test_2", "just_a_test_3",
                "just_a_test_4", "just_a_test_5"),
            panel_sample_id =
              c("just_a_test_1", "just_a_test_2", "just_a_test_3",
                "just_a_test_4", "just_a_test_5")),
          class = c("tbl_df", "tbl", "data.frame"),
          row.names = c(NA, -5L)),
      evaluation_table =
        structure(
          list(
            sample =
              c("just_a_test_1", "just_a_test_1", "just_a_test_1",
                "just_a_test_2", "just_a_test_2", "just_a_test_2",
                "just_a_test_3", "just_a_test_3", "just_a_test_3",
                "just_a_test_4", "just_a_test_4", "just_a_test_4",
                "just_a_test_5", "just_a_test_5", "just_a_test_5"),
            analyte =
              c("IgM", "IgG", "Pan-Ig", "IgM", "IgG", "Pan-Ig", "IgM", "IgG",
                "Pan-Ig", "IgM", "IgG", "Pan-Ig", "IgM", "IgG", "Pan-Ig"),
            target =
              c("RBD", "RBD", "RBD", "RBD", "RBD", "RBD", "RBD", "RBD", "RBD",
                "RBD", "RBD", "RBD", "RBD", "RBD", "RBD"),
            lot_number =
              c("20200101", "20200101", "20200101", "20200101", "20200101",
                "20200101", "20200101", "20200101", "20200101", "20200101",
                "20200101", "20200101", "20200101", "20200101", "20200101"),
            datetime_observation =
              structure(
                c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
                  NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
                  NA_real_, NA_real_, NA_real_),
                tzone = "UTC",
                class = c("POSIXct", "POSIXt")),
            qualitative_result =
              c(NA_character_, NA_character_, NA_character_, NA_character_,
                NA_character_, NA_character_, NA_character_, NA_character_,
                NA_character_, NA_character_, NA_character_, NA_character_,
                NA_character_, NA_character_, NA_character_),
            notes_and_anomalies =
              c(NA_character_, NA_character_, NA_character_, NA_character_,
                NA_character_, NA_character_, NA_character_, NA_character_,
                NA_character_, NA_character_, NA_character_, NA_character_,
                NA_character_, NA_character_, NA_character_)),
          row.names = c(NA, -15L),
          class = c("tbl_df", "tbl", "data.frame")
        )
    )

  # Save the panel data to a temporary location
  filepath_temp_panel <- tempfile(fileext = ".xlsx")
  write_panel_sheet(
    panel_sheet_data =
      build_panel_sheet(
        panel_name = "Just a test",
        panel_description = "A test case.",
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
    filepath = filepath_temp_panel
  )

  # Now let's put it all in using standard functions
  expect_equal(
    # Working example -- no error
    object =
      build_evaluation_sheet(
        evaluation_name = "Test Evaluation",
        evaluation_description = "A test...",
        developer = "Test Developer",
        assay = "The assay Name",
        lot_numbers = "20200101",
        panel_data =
          build_panel_sheet(
            panel_name = "Just a test",
            panel_description = "A test case.",
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
        panel_data_filepath = NA,
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = "RBD",
        qualitative_outcomes = c("Positive", "Negative"),
        semiquantitative_outcomes = NA_character_,
        quantitative_units = NA_character_,
        randomize = FALSE,
        blind = FALSE
      ),
    expected = good_output
  )
  # And again using the panel filepath
  expect_equal(
    # Working example -- no error
    object =
      build_evaluation_sheet(
        evaluation_name = "Test Evaluation",
        evaluation_description = "A test...",
        developer = "Test Developer",
        assay = "The assay Name",
        lot_numbers = "20200101",
        panel_data = NA,
        panel_data_filepath = filepath_temp_panel,
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = "RBD",
        qualitative_outcomes = c("Positive", "Negative"),
        semiquantitative_outcomes = NA_character_,
        quantitative_units = NA_character_,
        randomize = FALSE,
        blind = FALSE
      ),
    expected = good_output
  )
})

test_that("you can't provide panel and panel filepath", {
  expect_error(
    object =
      build_evaluation_sheet(
        evaluation_name = "Test Evaluation",
        evaluation_description = "A test...",
        developer = "Test Developer",
        assay = "The assay Name",
        lot_numbers = "20200101",
        panel_data =
          build_panel_sheet(
            panel_name = "Just a test",
            panel_description = "A test case.",
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
        panel_data_filepath = "bad_move_there_friend.xlsx",
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = "RBD",
        qualitative_outcomes = c("Positive", "Negative"),
        semiquantitative_outcomes = NA_character_,
        quantitative_units = NA_character_,
        randomize = FALSE,
        blind = FALSE
      ),
    regexp =
      "^You supplied both a panel_data object and a panel_data_filepath.*"
  )
})
