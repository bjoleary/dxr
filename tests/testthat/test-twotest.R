test_that("two-test series works", {
  result <- two_test_series(1, 0, .95, .95, .05)
  testthat::expect_equal(result$se_estimate, .95)
  testthat::expect_equal(result$sp_estimate, .95)
  result <- two_test_series(0.72, 0.96, .86, .99, .02)
  testthat::expect_equal(result$se_estimate, 0.6192)
  testthat::expect_equal(result$sp_estimate, 0.9996)
  result <- two_test_series(.97, .932, .88, .96, .05)
  testthat::expect_equal(plyr::round_any(result$ppv_estimate, .001), .943)
  testthat::expect_equal(plyr::round_any(result$npv_estimate, .01), .99)
  testthat::expect_equal(plyr::round_any(result$npv_estimate, .01), .99)
})

test_that("two test series risk analysis works", {
  # Example from calculator at
  # https://www.fda.gov/medical-devices/
  # covid-19-emergency-use-authorizations-medical-devices/
  # eua-authorized-serology-test-performance
  result <-
    risk_analysis_series(
      n = 10000,
      prev = 0.05,
      se1 = .970,
      sp1 = .932,
      se2 = .880,
      sp2 = .960
    )
  expect_equal(plyr::round_any(result$npv1, .001), .998)
  expect_equal(plyr::round_any(result$ppv1, .001), .429)
  expect_equal(plyr::round_any(result$npv2, .001), .993)
  expect_equal(plyr::round_any(result$ppv2, .001), .537)
  expect_equal(plyr::round_any(result$ppv_combined, .001), .943)
})
