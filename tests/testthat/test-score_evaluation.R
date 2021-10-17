test_that("scoring evaluations works", {
  skip("TODO")
  # We'll want to round-trip a known evaluation and check the counts.
})

test_that("score an example evaluation", {
  # For now, let's at least make sure the function runs and returns a tibble
  results_table <-
    score_evaluation(
      panel_data = nci_1,
      evaluation_data = test_eval
    )
  expect_s3_class(
    object = results_table,
    class = "tbl_df"
  )
})

test_that("we get proper errors", {
  nci_1_wrong <- nci_1
  nci_1_wrong$panel_table$qualitative_truth[[2]] <- "Not a real outcome"
  nci_1_wrong$panel_table$qualitative_truth[[5]] <- "Another bad one"
  expect_error(
    object =
      score_evaluation(
        panel_data = nci_1_wrong,
        evaluation_data = test_eval
      ),
    regexp = ".*Not a real outcome, Another bad one$"
  )
})
