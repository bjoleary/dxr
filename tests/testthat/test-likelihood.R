test_that("lr_pos works", {
  expect_equal(
    object =
      lr_pos(
        sensitivity_estimate = test_cases$tp / (test_cases$tp + test_cases$fn),
        specificity_estimate = test_cases$tn / (test_cases$tn + test_cases$fp),
        digits = 0.1
      ),
    expected = expected_results$lr_pos
  )
})

test_that("lr_neg works", {
  expect_equal(
    object =
      lr_neg(
        sensitivity_estimate = test_cases$tp / (test_cases$tp + test_cases$fn),
        specificity_estimate = test_cases$tn / (test_cases$tn + test_cases$fp),
        digits = 0.1
      ),
    expected = expected_results$lr_neg
  )
})
