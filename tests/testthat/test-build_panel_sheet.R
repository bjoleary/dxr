test_that("building a panel sheet works (case 1)", {
  case_1 <-
    build_panel_sheet(
      panel_name = "Case 1",
      panel_description = "A test case.",
      n_samples = 5L,
      sample_groups = "Case 1 Samples",
      sample_matrices = "Sputum",
      analytes = "SARS-CoV-2 Antigen",
      targets = "RBD",
      qualitative_outcomes = c("Positive", "Negative"),
      qualitative_comparators = "Authorized NAAT",
      semiquantitative_outcomes = NA,
      semiquantitative_comparators = NA,
      quantitative_units = NA,
      quantitative_comparators = NA
    )
  expect_equal(
    object = colnames(case_1$panel_table),
    expected =
      c("sample", "analyte", "target", "group", "matrix", "qualitative_result",
        "qualitative_comparator")
  )
  expect_equal(
    object = case_1$panel_table$sample[[1]],
    expected = "case_1_1"
  )
  expect_equal(
    object = case_1$panel_table$analyte %>% unique(),
    expected = "SARS-CoV-2 Antigen"
  )
  expect_equal(
    object = case_1$panel_table$target %>% unique(),
    expected = "RBD"
  )
  expect_equal(
    object = case_1$panel_table$group %>% unique(),
    expected = "Case 1 Samples"
  )
  expect_equal(
    object = case_1$panel_table$matrix %>% unique(),
    expected = "Sputum"
  )
  expect_true(
    all(is.na(case_1$panel_table$qualitative_result))
  )
  expect_equal(
    object = case_1$panel_table$qualitative_comparator %>% unique(),
    expected = "Authorized NAAT"
  )
})

test_that("building a panel sheet works (case 2)", {
  case_2 <-
    build_panel_sheet(
      panel_name = "Case 2",
      panel_description = "A test case.",
      n_samples = 5L,
      sample_groups = c("Group A", "Group B"),
      sample_matrices = c("Sputum", "NP Swab"),
      analytes = "SARS-CoV-2 Antigen",
      targets = "RBD",
      qualitative_outcomes = c("Positive", "Negative"),
      qualitative_comparators = c("Authorized NAAT"),
      semiquantitative_outcomes = c("Negative", "Weak Positive",
                                    "Medium Positive", "Strong Positive"),
      semiquantitative_comparators = "Authorized NAAT Ct Values",
      quantitative_units = "Unitless",
      quantitative_comparators = "Authorized NAAT Ct Values"
    )
  expect_equal(
    object = colnames(case_2$panel_table),
    expected =
      c("sample", "analyte", "target", "group", "matrix", "qualitative_result",
        "qualitative_comparator", "semiquantitative_result",
        "semiquantitative_comparator", "quantitative_result",
        "quantitative_units", "quantitative_comparator")
  )
  expect_equal(
    object = case_2$panel_table$sample[[5]],
    expected = "case_2_5"
  )
  expect_equal(
    object = case_2$panel_table$analyte %>% unique(),
    expected = "SARS-CoV-2 Antigen"
  )
  expect_equal(
    object = case_2$panel_table$target %>% unique(),
    expected = "RBD"
  )
  expect_true(
    all(is.na(case_2$panel_table$group))
  )
  expect_true(
    all(is.na(case_2$panel_table$matrix))
  )
  expect_true(
    all(is.na(case_2$panel_table$qualitative_result))
  )
  expect_equal(
    object = case_2$panel_table$qualitative_comparator %>% unique(),
    expected = "Authorized NAAT"
  )
  expect_true(
    all(is.na(case_2$panel_table$semiquantitative_result))
  )
  expect_equal(
    object = case_2$panel_table$semiquantitative_comparator %>% unique(),
    expected = "Authorized NAAT Ct Values"
  )
  expect_true(
    all(is.na(case_2$panel_table$quantitative_result))
  )
  expect_equal(
    object = case_2$panel_table$quantitative_units %>% unique(),
    expected = "Unitless"
  )
  expect_equal(
    object = case_2$panel_table$quantitative_comparator %>% unique(),
    expected = "Authorized NAAT Ct Values"
  )
})

test_that("proper errors are thrown", {
  # panel_name must not be null, must be char string length 1-------------------
  expect_error(
    object =
      build_panel_sheet(
        panel_name = NULL,
        panel_description = NA_character_,
        n_samples = 110L,
        sample_groups = c("Positive", "Negative"),
        sample_matrices = c("Serum", "Plasma"),
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = c("Method 1", "Method 2"),
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        semiquantitative_comparators = "Method 3",
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
      ),
    regexp = "panel_name"
  )
  expect_error(
    object =
      build_panel_sheet(
        panel_name = c("name_one", "name_two"),
        panel_description = NA_character_,
        n_samples = 110L,
        sample_groups = c("Positive", "Negative"),
        sample_matrices = c("Serum", "Plasma"),
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = c("Method 1", "Method 2"),
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        semiquantitative_comparators = "Method 3",
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
      ),
    regexp = "panel_name"
  )
  expect_error(
    object =
      build_panel_sheet(
        panel_name = 42,
        panel_description = NA_character_,
        n_samples = 110L,
        sample_groups = c("Positive", "Negative"),
        sample_matrices = c("Serum", "Plasma"),
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = c("Method 1", "Method 2"),
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        semiquantitative_comparators = "Method 3",
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
      ),
    regexp = "panel_name"
  )
  expect_error(
    object =
      build_panel_sheet(
        panel_name = NA_character_,
        panel_description = NA_character_,
        n_samples = 110L,
        sample_groups = c("Positive", "Negative"),
        sample_matrices = c("Serum", "Plasma"),
        analytes = c("IgM", "IgG", "Pan-Ig"),
        targets = c("Spike", "Nucleocapsid"),
        qualitative_outcomes = c("Positive", "Negative"),
        qualitative_comparators = c("Method 1", "Method 2"),
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        semiquantitative_comparators = "Method 3",
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
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
        semiquantitative_comparators = "Method 3",
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
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
        semiquantitative_comparators = "Method 3",
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
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
          semiquantitative_comparators = "Method 3",
          quantitative_units = NA_character_,
          quantitative_comparators = NA_character_
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
        semiquantitative_comparators = "Method 3",
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
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
        semiquantitative_comparators = "Method 3",
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
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
        semiquantitative_comparators = "Method 3",
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
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
        semiquantitative_comparators = "Method 3",
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
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
        semiquantitative_comparators = "Method 3",
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
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
        semiquantitative_comparators = "Method 3",
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
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
        semiquantitative_comparators = "Method 3",
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
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
        semiquantitative_comparators = "Method 3",
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
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
        semiquantitative_comparators = "Method 3",
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
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
        semiquantitative_comparators = "Method 3",
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
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
        semiquantitative_comparators = "Method 3",
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
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
        semiquantitative_comparators = "Method 3",
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
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
        semiquantitative_comparators = "Method 3",
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
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
        semiquantitative_comparators = "Method 3",
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
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
        semiquantitative_comparators = "Method 3",
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
      ),
    regexp = "qualitative_comparators"
)
  # semiquantitative_outcomes must be a character vector or NA -----------------
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
        semiquantitative_comparators = "Method 3",
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
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
        semiquantitative_comparators = "Method 3",
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
      ),
    regexp = "semiquantitative_outcomes"
  )
  # semiquantitative_comparators must be a character vector or NA --------------
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
        semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
        semiquantitative_comparators = list("hi"),
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
      ),
    regexp = "semiquantitative_comparators"
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
        semiquantitative_comparators = 42,
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
      ),
    regexp = "semiquantitative_comparators"
  )
  # Can't be NA unless outcomes is NA
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
        semiquantitative_comparators = NA_character_,
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
      ),
    regexp = "semiquantitative_comparators"
  )
  # Must be NA if outcomes is NA
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
        semiquantitative_outcomes = NA_character_,
        semiquantitative_comparators = c(NA_character_, "nope!"),
        quantitative_units = NA_character_,
        quantitative_comparators = NA_character_
      ),
    regexp = "semiquantitative_comparators"
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
        semiquantitative_comparators = c("Method 3", "Method 4"),
        quantitative_units = c("units 1", "units 2"),
        quantitative_comparators = "Method 5"
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
        semiquantitative_comparators = c("Method 3", "Method 4"),
        quantitative_units = 42,
        quantitative_comparators = "Method 5"
      ),
    regexp = "quantitative_units"
  )
  # quantitative_comparators must be a character vector or NA ------------------
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
        semiquantitative_outcomes = NA_character_,
        semiquantitative_comparators = NA_character_,
        quantitative_units = "unitless",
        quantitative_comparators = list("hi")
      ),
    regexp = "quantitative_comparators"
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
        semiquantitative_outcomes = NA_character_,
        semiquantitative_comparators = NA_character_,
        quantitative_units = "unitless",
        quantitative_comparators = 42
      ),
    regexp = "quantitative_comparators"
  )
  # Can't be NA unless units is NA
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
        semiquantitative_outcomes = NA_character_,
        semiquantitative_comparators = NA_character_,
        quantitative_units = "unitless",
        quantitative_comparators = NA_character_
      ),
    regexp = "quantitative_comparators"
  )
  # Must be NA if units is NA
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
        semiquantitative_outcomes = NA_character_,
        semiquantitative_comparators = NA_character_,
        quantitative_units = NA_character_,
        quantitative_comparators = c(NA_character_, "this shouldn't be here")
      ),
    regexp = "quantitative_comparators"
  )
})
