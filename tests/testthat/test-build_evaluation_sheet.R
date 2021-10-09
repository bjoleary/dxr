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
  skip("Need to write this test.")
})
