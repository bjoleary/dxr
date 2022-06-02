test_that("two_by_two works", {
  expect_equal(
    object =
      two_by_two(
        panel_data = nci_1,
        evaluation_data = test_eval
      ),
    expected =
      tibble::tribble(
        ~`SARS-COV-2 ELISA (IgG)`, ~`IgG+`, ~`IgG-`, ~`Total`,
        "IgG+", 27L, NA_integer_, 27L,
        "IgG Borderline", 2L, NA_integer_, 2L,
        "IgG-", 1L, 80L, 81L,
        "Total", 30L, 80L, 110L
      ),
    ignore_attr = TRUE
  )
})
