test_that("proportion_difference works", {
  # Worked example: Large numbers -- loc 1243 of Altman
  expect_equal(
    object =
      proportion_difference(
        true_calls_1 = 63,
        false_calls_1 = 30,
        true_calls_2 = 38,
        false_calls_2 = 54,
        interval = 0.95,
        digits = 0.1
      ),
    expected =
      list(
        estimate = 0.26437587657784,
        estimate_percent = "26.4%",
        lower = 0.121160441622926,
        lower_percent = "12.1%",
        upper = 0.392855732966957,
        upper_percent = "39.3%",
        difference_detected = TRUE,
        difference_detected_string =
          "A difference of 26.4% (95% CI: 12.1%; 39.3%) was detected.",
        confidence_interval = "(95% CI: 12.1%; 39.3%)",
        string_two_line = "26.4%\n(95% CI: 12.1%; 39.3%)",
        string = "26.4% (95% CI: 12.1%; 39.3%)")
  )

  # Worked example: Small numbers -- loc 1255 of Altman
  expect_equal(
    object =
      proportion_difference(
        true_calls_1 = 5,
        false_calls_1 = 51,
        true_calls_2 = 0,
        false_calls_2 = 29,
        interval = 0.95,
        digits = 0.1
      ),
    expected =
      list(
        estimate = 0.0892857142857143,
        estimate_percent = "8.9%",
        lower = -0.0381371479035369,
        lower_percent = "-3.8%",
        upper = 0.192560013855112,
        upper_percent = "19.3%",
        difference_detected = FALSE,
        difference_detected_string = "A difference was not detected.",
        confidence_interval = "(95% CI: -3.8%; 19.3%)",
        string_two_line = "8.9%\n(95% CI: -3.8%; 19.3%)",
        string = "8.9% (95% CI: -3.8%; 19.3%)"
      )
  )
})

test_that("proportion_difference_fraction works", {
  # Worked example: Large numbers -- loc 1243 of Altman
  expect_equal(
    object =
      proportion_difference_fraction(
        numerator_1 = 63,
        denominator_1 = 93,
        numerator_2 = 38,
        denominator_2 = 92,
        interval = 0.95,
        digits = 0.1
      ),
    expected =
      list(
        estimate = 0.26437587657784,
        estimate_percent = "26.4%",
        lower = 0.121160441622926,
        lower_percent = "12.1%",
        upper = 0.392855732966957,
        upper_percent = "39.3%",
        difference_detected = TRUE,
        difference_detected_string =
          "A difference of 26.4% (95% CI: 12.1%; 39.3%) was detected.",
        confidence_interval = "(95% CI: 12.1%; 39.3%)",
        string_two_line = "26.4%\n(95% CI: 12.1%; 39.3%)",
        string = "26.4% (95% CI: 12.1%; 39.3%)")
  )

  # Worked example: Small numbers -- loc 1255 of Altman
  expect_equal(
    object =
      proportion_difference_fraction(
        numerator_1 = 5,
        denominator_1 = 56,
        numerator_2 = 0,
        denominator_2 = 29,
        interval = 0.95,
        digits = 0.1
      ),
    expected =
      list(
        estimate = 0.0892857142857143,
        estimate_percent = "8.9%",
        lower = -0.0381371479035369,
        lower_percent = "-3.8%",
        upper = 0.192560013855112,
        upper_percent = "19.3%",
        difference_detected = FALSE,
        difference_detected_string = "A difference was not detected.",
        confidence_interval = "(95% CI: -3.8%; 19.3%)",
        string_two_line = "8.9%\n(95% CI: -3.8%; 19.3%)",
        string = "8.9% (95% CI: -3.8%; 19.3%)"
      )
  )
})
