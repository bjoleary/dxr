test_that("building a panel sheet works", {
  testthat::skip("Need to write this test.")
})

test_that("proper errors are thrown", {
  # panel_name must not be null, must be char string length 1-------------------
  expect_error(
    object =
      build_panel_sheet(
        panel_name = NULL,
        panel_description = NULL,
        n_samples = 110L,
        sample_groups = c("Positive", "Negative"),
        sample_matrices = c("Serum", "Plasma"),
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = c("Method 1", "Method 2"),
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        quantitative_units = NULL
      ),
    regexp = "panel_name"
  )
  expect_error(
    object =
      build_panel_sheet(
        panel_name = c("name_one", "name_two"),
        panel_description = NULL,
        n_samples = 110L,
        sample_groups = c("Positive", "Negative"),
        sample_matrices = c("Serum", "Plasma"),
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = c("Method 1", "Method 2"),
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        quantitative_units = NULL
      ),
    regexp = "panel_name"
  )
  expect_error(
    object =
      build_panel_sheet(
        panel_name = 42,
        panel_description = NULL,
        n_samples = 110L,
        sample_groups = c("Positive", "Negative"),
        sample_matrices = c("Serum", "Plasma"),
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = c("Method 1", "Method 2"),
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        quantitative_units = NULL
      ),
    regexp = "panel_name"
  )
  expect_error(
    object =
      build_panel_sheet(
        panel_name = NA_character_,
        panel_description = NULL,
        n_samples = 110L,
        sample_groups = c("Positive", "Negative"),
        sample_matrices = c("Serum", "Plasma"),
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = c("Method 1", "Method 2"),
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        quantitative_units = NULL
      ),
    regexp = "panel_name"
  )
  # panel_description must be char string length 1 or NULL or NA ---------------
  expect_error(
    object =
      build_panel_sheet(
        panel_name = "test",
        panel_description = c("name_one", "name_two"),
        n_samples = 110L,
        sample_groups = c("Positive", "Negative"),
        sample_matrices = c("Serum", "Plasma"),
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = c("Method 1", "Method 2"),
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        quantitative_units = NA_character_
      ),
    regexp = "panel_description"
  )
  expect_error(
    object =
      build_panel_sheet(
        panel_name = "test",
        panel_description = 42,
        n_samples = 110L,
        sample_groups = c("Positive", "Negative"),
        sample_matrices = c("Serum", "Plasma"),
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = c("Method 1", "Method 2"),
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        quantitative_units = NA_character_
      ),
    regexp = "panel_description"
  )
  # n_samples parameter must be a number greater than 0 ------------------------
  expect_error(
    object =
      suppressWarnings(
        build_panel_sheet(
          panel_name = "test",
          panel_description = NA_character_,
          n_samples = -110,
          sample_groups = c("Positive", "Negative"),
          sample_matrices = c("Serum", "Plasma"),
          analytes = c("IgM", "IgG", "Pan-Ig"),
          targets = c("Spike", "Nucleocapsid"),
          qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = c("Method 1", "Method 2"),
          semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
          quantitative_units = NULL
        )
      ),
    regexp = "n_samples.*0"
  )
  expect_error(
    object =
      build_panel_sheet(
        panel_name = "test",
        panel_description = NA_character_,
        n_samples = "110",
        sample_groups = c("Positive", "Negative"),
        sample_matrices = c("Serum", "Plasma"),
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = c("Method 1", "Method 2"),
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        quantitative_units = NA_character_
      ),
    regexp = "numeric.*n_samples"
  )
  expect_error(
    object =
      build_panel_sheet(
        panel_name = "test",
        panel_description = NA_character_,
        n_samples = NA_integer_,
        sample_groups = c("Positive", "Negative"),
        sample_matrices = c("Serum", "Plasma"),
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = c("Method 1", "Method 2"),
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        quantitative_units = NA_character_
      ),
    regexp = "n_samples.*0"
  )
  expect_warning(
    object =
      build_panel_sheet(
        panel_name = "test",
        panel_description = NA_character_,
        n_samples = 113.212,
        sample_groups = c("Positive", "Negative"),
        sample_matrices = c("Serum", "Plasma"),
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = c("Method 1", "Method 2"),
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        quantitative_units = NA_character_
      ),
    regexp = "Converting n_samples.*"
  )
  # sample_groups parameter should be a character vector -----------------------
  expect_error(
    object =
      build_panel_sheet(
        panel_name = "test",
        panel_description = NA_character_,
        n_samples = 110L,
        sample_groups = NA_character_,
        sample_matrices = c("Serum", "Plasma"),
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = c("Method 1", "Method 2"),
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        quantitative_units = NA_character_
      ),
    regexp = "sample_groups"
  )
  expect_error(
    object =
      build_panel_sheet(
        panel_name = "test",
        panel_description = NA_character_,
        n_samples = 110L,
        sample_groups = 113,
        sample_matrices = c("Serum", "Plasma"),
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = c("Method 1", "Method 2"),
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        quantitative_units = NA_character_
      ),
    regexp = "sample_groups"
  )
  # sample_matrices parameter should be a character vector ---------------------
  expect_error(
    object =
      build_panel_sheet(
        panel_name = "test",
        panel_description = NA_character_,
        n_samples = 110L,
        sample_groups = c("Positive", "Negative"),
        sample_matrices = NA_character_,
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = c("Method 1", "Method 2"),
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        quantitative_units = NA_character_
      ),
    regexp = "sample_matrices"
  )
  expect_error(
    object =
      build_panel_sheet(
        panel_name = "test",
        panel_description = NA_character_,
        n_samples = 110L,
        sample_groups = c("Positive", "Negative"),
        sample_matrices = 0,
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = c("Method 1", "Method 2"),
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        quantitative_units = NA_character_
      ),
    regexp = "sample_matrices"
  )
  # analytes is a character vector with at least one level ---------------------
  expect_error(
    object =
      build_panel_sheet(
        panel_name = "test",
        panel_description = NA_character_,
        n_samples = 110L,
        sample_groups = c("Positive", "Negative"),
        sample_matrices = c("Serum", "Plasma"),
        analytes = NA_character_,
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = c("Method 1", "Method 2"),
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        quantitative_units = NA_character_
      ),
    regexp = "analytes"
  )
  expect_error(
    object =
      build_panel_sheet(
        panel_name = "test",
        panel_description = NA_character_,
        n_samples = 110L,
        sample_groups = c("Positive", "Negative"),
        sample_matrices = c("Serum", "Plasma"),
        analytes = 0,
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = c("Method 1", "Method 2"),
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        quantitative_units = NA_character_
      ),
    regexp = "analytes"
  )
  # targets is a character vector with at least one level ----------------------
  expect_error(
    object =
      build_panel_sheet(
        panel_name = "test",
        panel_description = NA_character_,
        n_samples = 110L,
        sample_groups = c("Positive", "Negative"),
        sample_matrices = c("Serum", "Plasma"),
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = NA_character_,
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = c("Method 1", "Method 2"),
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        quantitative_units = NA_character_
      ),
    regexp = "targets"
  )
  expect_error(
    object =
      build_panel_sheet(
        panel_name = "test",
        panel_description = NA_character_,
        n_samples = 110L,
        sample_groups = c("Positive", "Negative"),
        sample_matrices = c("Serum", "Plasma"),
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = c("Spike", "Nucleocapsid"),
        targets = 0,
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = c("Method 1", "Method 2"),
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        quantitative_units = NA_character_
      ),
    regexp = "targets"
  )
  # qualitative_outcomes is a character vector with at least one level ---------
  expect_error(
    object =
      build_panel_sheet(
        panel_name = "test",
        panel_description = NA_character_,
        n_samples = 110L,
        sample_groups = c("Positive", "Negative"),
        sample_matrices = c("Serum", "Plasma"),
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = NA_character_,
        qualitative_comparators = c("Method 1", "Method 2"),
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        quantitative_units = NA_character_
      ),
    regexp = "qualitative_outcomes"
  )
  expect_error(
    object =
      build_panel_sheet(
        panel_name = "test",
        panel_description = NA_character_,
        n_samples = 110L,
        sample_groups = c("Positive", "Negative"),
        sample_matrices = c("Serum", "Plasma"),
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = 0,
        qualitative_comparators = c("Method 1", "Method 2"),
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        quantitative_units = NA_character_
      ),
    regexp = "qualitative_outcomes"
  )
  # qualitative_comparators is a character vector with at least 1 level --------
  expect_error(
    object =
      build_panel_sheet(
        panel_name = "test",
        panel_description = NA_character_,
        n_samples = 110L,
        sample_groups = c("Positive", "Negative"),
        sample_matrices = c("Serum", "Plasma"),
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = NA_character_,
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        quantitative_units = NA_character_
      ),
    regexp = "qualitative_comparators"
  )
  expect_error(
    object =
      build_panel_sheet(
        panel_name = "test",
        panel_description = NA_character_,
        n_samples = 110L,
        sample_groups = c("Positive", "Negative"),
        sample_matrices = c("Serum", "Plasma"),
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = 0,
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        quantitative_units = NA_character_
      ),
    regexp = "qualitative_comparators"
)
  # semiquantitative_outcomes must be a character vector or NULL ---------------
  expect_error(
    object =
      build_panel_sheet(
        panel_name = "test",
        panel_description = NA_character_,
        n_samples = 110L,
        sample_groups = c("Positive", "Negative"),
        sample_matrices = c("Serum", "Plasma"),
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = "Method 1",
        semiquantitative_outcomes = list("hi"),
        quantitative_units = NA_character_
      ),
    regexp = "semiquantitative_outcomes"
  )
  expect_error(
    object =
      build_panel_sheet(
        panel_name = "test",
        panel_description = NA_character_,
        n_samples = 110L,
        sample_groups = c("Positive", "Negative"),
        sample_matrices = c("Serum", "Plasma"),
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = c("Method 1", "Method 2"),
        semiquantitative_outcomes = 42,
        quantitative_units = NA_character_
      ),
    regexp = "semiquantitative_outcomes"
  )
  # quantitative_units must be a character string (not a vector) or NULL -------
  expect_error(
    object =
      build_panel_sheet(
        panel_name = "test",
        panel_description = NA_character_,
        n_samples = 110L,
        sample_groups = c("Positive", "Negative"),
        sample_matrices = c("Serum", "Plasma"),
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = c("Method 1", "Method 2"),
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        quantitative_units = c("units 1", "units 2")
      ),
    regexp = "quantitative_units"
  )
  expect_error(
    object =
      build_panel_sheet(
        panel_name = "test",
        panel_description = NA_character_,
        n_samples = 110L,
        sample_groups = c("Positive", "Negative"),
        sample_matrices = c("Serum", "Plasma"),
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = c("Method 1", "Method 2"),
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        quantitative_units = 42
      ),
    regexp = "quantitative_units"
  )
})
