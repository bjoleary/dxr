test_that("get_panel_metadata works", {
  expected_output <-
    list(
      panel_name = "NCI Panel 1",
      panel_description =
        paste0(
          "A panel used in Spring - Summer 2020 to evaluate serology assays ",
          "for SARS-CoV-2. Use in this package for testing purposes only. ",
          "See https://open.fda.gov/apis/device/covid19serology/ for more ",
          "information."
        ),
      n_samples = 110L,
      sample_groups = c("Positive", "Negative", "HIV+"),
      sample_matrices = c("Serum", "Plasma"),
      analytes = c("IgM", "IgG", "Pan-Ig"),
      targets = "Antibodies to SARS-CoV-2 Spike Protein Antigen",
      qualitative_outcomes = c("Positive", "Negative"),
      qualitative_comparators =
        c(paste("PCR-confirmed and positive on CDC Spike Antigen Assays",
                "and Krammer RBD assay at NCI"),
          "Collected prior to 2020"),
      semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
      semiquantitative_comparators = "CDC Spike Antigen Assay",
      quantitative_units = NA_character_,
      quantitative_comparators = NA_character_
    )
  expect_equal(
    object = get_panel_metadata(nci_1),
    expected = expected_output
  )
})

test_that("get_evaluation_metadata works", {
  expected_output <-
    list(
      evaluation_name = "Test of NCI Panel 1 Evaluation",
      evaluation_description = "Nothing",
      developer = "Euroimmun",
      assay = "SARS-COV-2 ELISA (IgG)",
      lot_numbers = "E200330DT",
      analytes = "IgG",
      targets = "Spike",
      qualitative_outcomes = c("Positive", "Borderline", "Negative"),
      semiquantitative_outcomes = NA_character_,
      quantitative_units = NA_character_,
      blinded = FALSE
    )
  expect_equal(
    object = get_evaluation_metadata(test_eval),
    expected = expected_output
  )
})

test_that("replace_pos_neg works", {
  expect_equal(
    object =
      replace_pos_neg(c("IgM Negative", "IgM Positive", "IgM Borderline")),
    expected =
      c("IgM-", "IgM+", "IgM Borderline")
  )
  expect_equal(
    object =
      replace_pos_neg(c(" Negative", " Positive", "Positive", "Negative")),
    expected =
      c("-", "+", "Positive", "Negative")
  )
})

test_that("crossed_outcomes works", {
  expect_equal(
    object =
      crossed_outcomes(
        analytes = c("IgM", "IgG"),
        qualitative_outcomes = c("Positive", "Negative")
      ),
    expected = c("IgM+, IgG+", "IgM-, IgG+", "IgM+, IgG-", "IgM-, IgG-")
  )
  expect_equal(
    object =
      crossed_outcomes(
        analytes = "IgG",
        qualitative_outcomes = c("Positive", "Equivocal", "Negative")
      ),
    expected = c("IgG+", "IgG Equivocal", "IgG-")
  )
})

test_that("lengthen works", {
  expect_equal(
    object = lengthen(vec = c(1:5), desired_length = 10L),
    expected = c(c(1:5), NA, NA, NA, NA, NA)
  )
  expect_equal(
    object = lengthen(vec = c(1:5), desired_length = 5L),
    expected = c(1:5)
  )
})

test_that("validation_string works", {
  skip("TODO")
})
