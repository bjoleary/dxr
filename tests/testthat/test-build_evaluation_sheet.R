test_that("evaluations and panels must have compatable analytes", {
  expect_error(
    object =
      build_evaluation_sheet(
        evaluation_name = "Test Evaluation",
        evaluation_description = "A test...",
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

test_that("building the evaluation sheet works", {
  skip("Need to write this test.")
})
