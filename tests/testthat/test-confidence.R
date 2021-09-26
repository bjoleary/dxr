test_that("confidence works", {
  # Worked example: large numbers (Altman loc 1189)
  expect_equal(
    object =
      confidence(
        true_calls = 81,
        false_calls = 263 - 81,
        interval = 0.95
      ),
    expected =
      list(lower_bound = 0.255288519878274, upper_bound = 0.3662095769828)
  )

  # Worked examples: small numbers (Altman loc 1189)
  expect_equal(
    object =
      confidence(
        true_calls = 1,
        false_calls = 28,
        interval = 0.95
      ),
    expected =
      list(lower_bound = 0.00611321429276265, upper_bound = 0.171755218793203)
  )
  expect_equal(
    object =
      confidence(
        true_calls = 0,
        false_calls = 20,
        interval = 0.95
      ),
    expected =
      list(lower_bound = 0, upper_bound = 0.161125158052819)
  )
})
