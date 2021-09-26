test_that("ppv works", {
  expect_equal(
    object =
      ppv(
        sensitivity = test_cases$tp / (test_cases$tp + test_cases$fn),
        specificity = test_cases$tn / (test_cases$tn + test_cases$fp),
        prevalence = 0.05
      ),
    expected = expected_results$ppv_estimate
  )
})

test_that("npv works", {
  expect_equal(
    object =
      npv(
        sensitivity = test_cases$tp / (test_cases$tp + test_cases$fn),
        specificity = test_cases$tn / (test_cases$tn + test_cases$fp),
        prevalence = 0.05
      ),
    expected = expected_results$npv_estimate
  )
})

test_that("ppv_with_confidence works", {
  expect_equal(
    object =
      ppv_with_confidence(
        sensitivity =
          sensitivity(
            true_positives = test_cases$tp,
            false_negatives = test_cases$fn,
            interval = 0.95
          ),
        specificity =
          specificity(
            true_negatives = test_cases$tn,
            false_positives = test_cases$fp,
            interval = 0.95
          ),
        prevalence = 0.05,
        interval = 0.95
      ) %>%
      tibble::as_tibble() %>%
      dplyr::rename_with(.fn = ~paste0("ppv_", .x)) %>%
      dplyr::select(-dplyr::contains("two_line")),
    expected =
      expected_results %>%
      dplyr::select(
        dplyr::contains("ppv_")
      )
  )
})

test_that("npv_with_confidence works", {
  expect_equal(
    object =
      npv_with_confidence(
        sensitivity =
          sensitivity(
            true_positives = test_cases$tp,
            false_negatives = test_cases$fn,
            interval = 0.95
          ),
        specificity =
          specificity(
            true_negatives = test_cases$tn,
            false_positives = test_cases$fp,
            interval = 0.95
          ),
        prevalence = 0.05,
        interval = 0.95
      ) %>%
      tibble::as_tibble() %>%
      dplyr::rename_with(.fn = ~paste0("npv_", .x)) %>%
      dplyr::select(-dplyr::contains("two_line")),
    expected =
      expected_results %>%
      dplyr::select(
        dplyr::contains("npv_")
      )
  )
})
