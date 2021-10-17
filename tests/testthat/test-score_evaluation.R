test_that("scoring evaluations works", {
  test_eval_outcomes <-
    score_evaluation(
      panel_data = nci_1,
      evaluation_data = test_eval
    )
  expect_equal(
    object =  test_eval_outcomes$qualitative_match %>% sum(),
    expected = 107
  )
  expect_equal(
    object =
      test_eval_outcomes$qualitative_outcome_strict %>%
      stringr::str_detect("True Negative") %>%
      sum(),
    expected = 80
  )
  expect_equal(
    object =
      test_eval_outcomes$qualitative_outcome_strict %>%
      stringr::str_detect("True Positive") %>%
      sum(),
    expected = 27
  )
  expect_equal(
    object =
      test_eval_outcomes$qualitative_outcome_strict %>%
      stringr::str_detect("False Negative") %>%
      sum(),
    expected = 3
  )
  expect_equal(
    object =
      test_eval_outcomes$qualitative_outcome_strict %>%
      stringr::str_detect("False Positive") %>%
      sum(),
    expected = 0
  )
})

test_that("score an example evaluation", {
  # Let's make sure the function runs and returns a tibble
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
