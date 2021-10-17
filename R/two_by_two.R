#' Two by Two
#'
#' Generate the "two by two" table of results observed in the evaluation vs.
#' those expected based on the ground truth in the panel.
#'
#' @param panel_data The result of \code{read_panel()}.
#' @param evaluation_data The result of \code{read_evaluation()}.
#'
#' @return A two-by-two table of outcomes
#' @export
#'
#' @examples
#' two_by_two(dxr::example_panel_1, dxr::example_evaluation_1)
two_by_two <- function(panel_data, evaluation_data) {
  line_data <-
    score_evaluation(
      panel_data = panel_data,
      evaluation_data = evaluation_data
    )
  panel_metadata <- get_panel_metadata(panel_data = panel_data)
  evaluation_metadata <-
    get_evaluation_metadata(evaluation_data = evaluation_data)

  line_data %>%
    dplyr::group_by(
      .data$panel_sample_id
    ) %>%
    # Paste the analyte and the result together to form a string
    dplyr::summarise(
      qualitative_result =
        paste(.data$analyte, .data$qualitative_result) %>%
        paste(collapse = ", ") %>%
        replace_pos_neg(),
      qualitative_truth =
        paste(.data$analyte, .data$qualitative_truth) %>%
        paste(collapse = ", ") %>%
        replace_pos_neg(),
    ) %>%
    # Group by result and truth, then count the associated results
    dplyr::group_by(
      .data$qualitative_result,
      .data$qualitative_truth
    ) %>%
    dplyr::tally() %>%
    # Expand the table to include all possible permutations of results and
    # truths
    tidyr::complete(
      qualitative_result =
        crossed_outcomes(
          evaluation_metadata$analytes,
          evaluation_metadata$qualitative_outcomes
        ),
      qualitative_truth =
        crossed_outcomes(
          evaluation_metadata$analytes,
          panel_metadata$qualitative_outcomes
        )
    ) %>%
    # Remove duplicates
    dplyr::distinct() %>%
    # Make things into factors, and set the factor levels according to the
    # metadata. This enables us to sort things in the order they appear in
    # the metadata.
    dplyr::mutate(
      qualitative_result =
        .data$qualitative_result %>%
        forcats::as_factor() %>%
        forcats::lvls_expand(
          crossed_outcomes(
            evaluation_metadata$analytes,
            evaluation_metadata$qualitative_outcomes
          )
        ),
      qualitative_truth =
        .data$qualitative_truth %>%
        forcats::as_factor() %>%
        forcats::lvls_expand(
          crossed_outcomes(
            evaluation_metadata$analytes,
            panel_metadata$qualitative_outcomes
          )
        )
    ) %>%
    dplyr::arrange(
      .data$qualitative_result,
      .data$qualitative_truth
    ) %>%
    # Pivot the truth out to the right as columns, keeping the results as rows
    tidyr::pivot_wider(
      names_from = .data$qualitative_truth,
      values_from = .data$n
    ) %>%
    # Add a total row and column
    janitor::adorn_totals(
      where = c("row", "col"),
      na.rm = TRUE,
      name = "Total"
    ) %>%
    # Add the assay name
    dplyr::rename(
      !!evaluation_metadata$assay := .data$qualitative_result
    )
}
