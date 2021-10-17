#' Calculate Performance
#'
#' **UNDER DEVELOPMENT**
#'
#' Generate a summary statistics table.
#'
#' @param panel_data The result of \code{read_panel()}.
#' @param evaluation_data The result of \code{read_evaluation()}.
#'
#' @return A table of summary statistics.
#' @export
#'
#' @examples
#' calculate_performance(dxr::example_panel_1, dxr::example_evaluation_1)
calculate_performance <- function(panel_data, evaluation_data) {
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
      .data$analyte,
      .data$qualitative_truth,
      .data$qualitative_result
    ) %>%
    dplyr::tally() %>%
    tidyr::complete(
      analyte = evaluation_metadata$analytes,
      qualitative_truth = panel_metadata$qualitative_outcomes,
      qualitative_result = evaluation_metadata$qualitative_outcomes,
      fill = list("n" = NA)
    ) %>%
    dplyr::mutate(
      analyte =
        .data$analyte %>%
        forcats::as_factor() %>%
        forcats::lvls_expand(
          f = .,
          new_levels = evaluation_metadata$analytes
        ),
      qualitative_result =
        .data$qualitative_result %>%
        forcats::as_factor() %>%
        forcats::lvls_expand(
          f = .,
          new_levels = evaluation_metadata$qualitative_outcomes
        ),
      qualitative_truth =
        .data$qualitative_truth %>%
        forcats::as_factor() %>%
        forcats::lvls_expand(
          f = .,
          new_levels = panel_metadata$qualitative_outcomes
        )
    ) %>%
    dplyr::distinct() %>%
    dplyr::arrange(
      .data$qualitative_truth,
      .data$qualitative_result
    ) %>%
    tidyr::pivot_wider(
      names_from = .data$qualitative_truth,
      values_from = .data$n
    )
}
