test_that("calculate_performance works", {
  expect_equal(
    object = calculate_performance(nci_1, test_eval),
    expected =
      list(
        ppa =
          structure(
            list(
              analyte = "IgG", ppa_estimate = 0.9,
              ppa_estimate_percent = "90.0%", ppa_ratio = "27/30",
              ppa_lower = 0.743789174208159,
              ppa_lower_percent = "74.4%", ppa_upper = 0.965400111252666,
              ppa_upper_percent = "96.5%",
              ppa_confidence_interval = "(95% CI: 74.4%; 96.5%)",
              ppa_string_two_line = "90.0% (27/30) \n(95% CI: 74.4%; 96.5%)",
              ppa_string = "27/30 = 90.0% (95% CI: 74.4%; 96.5%)"),
            row.names = c(NA, -1L),
            class = c("tbl_df", "tbl", "data.frame")),
        npa =
          structure(
            list(
              analyte = "IgG", npa_estimate = 1, npa_estimate_percent = "100%",
              npa_ratio = "80/80", npa_lower = 0.954181870464473,
              npa_lower_percent = "95.4%",
              npa_upper = 1, npa_upper_percent = "100%",
              npa_confidence_interval = "(95% CI: 95.4%; 100%)",
              npa_string_two_line = "100% (80/80) \n(95% CI: 95.4%; 100%)",
              npa_string = "80/80 = 100% (95% CI: 95.4%; 100%)"),
            row.names = c(NA, -1L),
            class = c("tbl_df", "tbl", "data.frame")),
        summary =
          structure(
            list(
              performance_measure = c("IgG PPA", "IgG NPA"),
              estimate = c("27/30 = 90.0% (95% CI: 74.4%; 96.5%)",
                           "80/80 = 100% (95% CI: 95.4%; 100%)")),
            row.names = c(NA, -2L),
            class = c("tbl_df", "tbl", "data.frame")
            )
      )
  )
})

test_that("calculate_performance works with multiple analytes", {
  skip("TODO")
})
