#' Score Evaluation
#'
#' @param panel_data The panel data associated with the evaluation, as read in
#'   by \code{read_panel()}.
#' @param evaluation_data The evaluation data associated with the evaluation, as
#'   read in by \code{read_evaluation()}.
#'
#' @return A table of scored results with qualitative results from the
#'   evaluation compared to ground truth in the panel.
#' @export
#'
score_evaluation <- function(panel_data, evaluation_data) {
  # TODO: Enable scoring by target as well for tests where the measurand is the
  # combination of the analyte and the target.
  complete_table <-
    dplyr::left_join(
      x =
        dplyr::left_join(
          x = evaluation_data$sample_blinding,
          y = evaluation_data$evaluation_table,
          by = c("evaluation_sample_id" = "sample")
        ) %>%
        dplyr::rename(evaluation_target = .data$target),
      y =
        panel_data$panel_table %>%
        dplyr::rename(comparator_target = .data$target),
      by =
        c(
          "panel_sample_id" = "sample",
          "analyte" = "analyte"
        )
    )

  complete_table <- score_qualitative(complete_table)

  # TODO: Need to make this comprehensive and decide on an order...
  column_order <-
    c(
      "panel_sample_id",
      "evaluation_sample_id",
      "lot_number",
      "matrix",
      "group",
      "analyte",
      "evaluation_target",
      "datetime_observation",
      "qualitative_result",
      "qualitative_truth",
      "qualitative_match",
      "qualitative_outcome_strict",
      "semiquantitative_result",
      "semiquantitative_truth",
      "semiquantitative_match",
      "notes_and_anomalies",
      "comparator_target",
      "qualitative_comparator",
      "semiquantitative_comparator"
    )
  column_order <- column_order[column_order %in% colnames(complete_table)]

  complete_table <-
    complete_table %>%
    dplyr::select(
      dplyr::all_of(column_order),
      dplyr::everything()
    )

}

score_qualitative <- function(results_table) {
  if (!all(results_table$qualitative_truth %in% c("Positive", "Negative"))) {
    unsupported_outcomes <-
      results_table$qualitative_truth %>%
      unique()
    unsupported_outcomes <-
      unsupported_outcomes[
        !(unsupported_outcomes %in% c("Positive", "Negative"))
      ]
    stop(
      "At this time, dxr can only score qualitative results when the ",
      "qualitative ground truths in ",
      "the panel are limited to \"Positive\" and \"Negative\". Your panel ",
      "includes one or more additional levels: ",
      unsupported_outcomes
    )
  }
  results_table <-
    results_table %>%
    dplyr::mutate(
      qualitative_match =
        .data$qualitative_result == .data$qualitative_truth
    ) %>%
    dplyr::mutate(
      qualitative_outcome_strict =
        dplyr::case_when(
          # First, take care of missing results
          is.na(.data$qualitative_result) ~ NA_character_,
          # Now, the ones that were correct
          .data$qualitative_match == TRUE ~
            paste("True", .data$qualitative_truth),
          # If it is not true, continue:
          # If the result is not equivocal or similar:
          .data$qualitative_result %in% c("Positive", "Negative") ~
            paste("False", .data$qualitative_result),
          # If that is not true, continue, flipping the qualitative truth:
          .data$qualitative_truth == "Positive" ~ "False Negative",
          .data$qualitative_truth == "Negative" ~ "False Positive",
          # If we get down here, we're really in trouble...
          TRUE ~ "Unknown"
        )
    )

  # If we weren't able to score every outcome, stop and throw an error.
  if ("Unknown" %in% results_table$qualitative_outcome_strict) {
    if (requireNamespace("utils", quietly = TRUE)) {
      problematic_results <-
        results_table %>%
        dplyr::filter(
          .data$qualitative_outcome_strict == "Unknown"
        )
      stop(
        "Unable to score one or more results: \n",
        paste(
          utils::capture.output(print(problematic_results)),
          collapse = "\n"
        )
      )
    } else {
      stop(
        "Unable to score one or more results. Install the \"utils\" package ",
        "for a more informative error message."
      )
    }
  }
  results_table
}

check_eval_errors <- function(panel_data, evaluation_data) {
  # TODO: Check for showstoppers before scoring
}

check_eval_warnings <- function(panel_data, evaluation_data) {
  # TODO: Check for warnings before scoring
}
