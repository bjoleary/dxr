test_that("agreement works", {
  # PPA
  expect_equal(
    agreement(
      true_calls = test_cases$tp,
      false_calls = test_cases$fn
    ) %>%
      tibble::as_tibble() %>%
      dplyr::rename_with(.fn = ~paste0("ppa_", .)) %>%
      dplyr::select(-dplyr::contains("two_line")),
    expected =
      expected_results %>%
      dplyr::select(
        dplyr::contains("ppa_")
      )
  )

  # NPA
  expect_equal(
    agreement(
      true_calls = test_cases$tn,
      false_calls = test_cases$fp
    ) %>%
      tibble::as_tibble() %>%
      dplyr::rename_with(.fn = ~paste0("npa_", .)) %>%
      dplyr::select(-dplyr::contains("two_line")),
    expected =
      expected_results %>%
      dplyr::select(
        dplyr::contains("npa_")
      )
  )
})

test_that("agreement_fraction works", {
  # PPA
  expect_equal(
    agreement_fraction(
      numerator = test_cases$tp,
      denominator = test_cases$tp + test_cases$fn
    ) %>%
      tibble::as_tibble() %>%
      dplyr::rename_with(.fn = ~paste0("ppa_", .)) %>%
      dplyr::select(-dplyr::contains("two_line")),
    expected =
      expected_results %>%
      dplyr::select(
        dplyr::contains("ppa_")
      )
  )

  # NPA
  expect_equal(
    agreement_fraction(
      numerator = test_cases$tn,
      denominator = test_cases$tn + test_cases$fp
    ) %>%
      tibble::as_tibble() %>%
      dplyr::rename_with(.fn = ~paste0("npa_", .)) %>%
      dplyr::select(-dplyr::contains("two_line")),
    expected =
      expected_results %>%
      dplyr::select(
        dplyr::contains("npa_")
      )
  )
})

test_that("sensitivity works", {
  expect_equal(
    sensitivity(
      true_positives = test_cases$tp,
      false_negatives = test_cases$fn
    ) %>%
      tibble::as_tibble() %>%
      dplyr::rename_with(.fn = ~paste0("ppa_", .)) %>%
      dplyr::select(-dplyr::contains("two_line")),
    expected =
      expected_results %>%
      dplyr::select(
        dplyr::contains("ppa_")
      )
  )
})

test_that("specificity works", {
  expect_equal(
    specificity(
      true_negatives = test_cases$tn,
      false_positives = test_cases$fp
    ) %>%
      tibble::as_tibble() %>%
      dplyr::rename_with(.fn = ~paste0("npa_", .)) %>%
      dplyr::select(-dplyr::contains("two_line")),
    expected =
      expected_results %>%
      dplyr::select(
        dplyr::contains("npa_")
      )
  )
})

test_that("sensitivity_fraction works", {
  expect_equal(
    sensitivity_fraction(
      numerator = test_cases$tp,
      denominator = test_cases$tp + test_cases$fn
    ) %>%
      tibble::as_tibble() %>%
      dplyr::rename_with(.fn = ~paste0("ppa_", .)) %>%
      dplyr::select(-dplyr::contains("two_line")),
    expected =
      expected_results %>%
      dplyr::select(
        dplyr::contains("ppa_")
      )
  )
})

test_that("specificity_fraction works", {
  expect_equal(
    specificity_fraction(
      numerator = test_cases$tn,
      denominator = test_cases$tn + test_cases$fp
    ) %>%
      tibble::as_tibble() %>%
      dplyr::rename_with(.fn = ~paste0("npa_", .)) %>%
      dplyr::select(-dplyr::contains("two_line")),
    expected =
      expected_results %>%
      dplyr::select(
        dplyr::contains("npa_")
      )
  )
})

test_that("agreement fractions work with big_format", {
  expect_equal(
    agreement_fraction(1000, 10^8, format_big = TRUE)$string,
    "1,000/100,000,000 = 0.0% (95% CI: 0.0%; 0.0%)"
  )
  expect_equal(
    agreement(1000, 1000, format_big = TRUE)$string,
    "1,000/2,000 = 50.0% (95% CI: 47.8%; 52.2%)"
  )
  expect_equal(
    specificity(1000, 1000, format_big = TRUE)$string,
    "1,000/2,000 = 50.0% (95% CI: 47.8%; 52.2%)"
  )
  expect_equal(
    specificity(1000, 1000, format_big = TRUE)$string,
    "1,000/2,000 = 50.0% (95% CI: 47.8%; 52.2%)"
  )
  expect_equal(
    agreement_fraction(1000, 2000, format_big = TRUE)$string,
    "1,000/2,000 = 50.0% (95% CI: 47.8%; 52.2%)"
  )
  expect_equal(
    sensitivity_fraction(1000, 2000, format_big = TRUE)$string,
    "1,000/2,000 = 50.0% (95% CI: 47.8%; 52.2%)"
  )
  expect_equal(
    specificity_fraction(1000, 2000, format_big = TRUE)$string,
    "1,000/2,000 = 50.0% (95% CI: 47.8%; 52.2%)"
  )

})
