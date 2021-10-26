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

  results <-
    line_data %>%
    dplyr::group_by(
      .data$analyte,
      .data$qualitative_outcome_strict
    ) %>%
    dplyr::tally() %>%
    tidyr::complete(
      analyte = evaluation_metadata$analytes,
      qualitative_outcome_strict =
        c(
          paste("True", panel_metadata$qualitative_outcomes),
          paste("False", panel_metadata$qualitative_outcomes)
        ),
      fill = list("n" = 0)
    ) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(
      names_from = .data$qualitative_outcome_strict,
      values_from = .data$n
    ) %>%
    janitor::clean_names() %>%
    as.list()

  ppa <-
    sensitivity(
      true_positives = results$true_positive,
      false_negatives = results$false_negative
    ) %>%
    tibble::as_tibble() %>%
    dplyr::rename_all(
      .funs = ~ paste0("ppa_", .x)
    ) %>%
    dplyr::bind_cols(
      analyte = results$analyte
    ) %>%
    dplyr::select(
      .data$analyte,
      dplyr::everything()
    )

  npa <-
    specificity(
      true_negatives = results$true_negative,
      false_positives = results$false_positive
    ) %>%
    tibble::as_tibble() %>%
    dplyr::rename_all(
      .funs = ~ paste0("npa_", .x)
    ) %>%
    dplyr::bind_cols(
      analyte = results$analyte
    ) %>%
    dplyr::select(
      .data$analyte,
      dplyr::everything()
    )

  # TODO: PPV and NPV with prevalence

  summary <-
    dplyr::bind_rows(
      ppa %>%
        dplyr::select(
          .data$analyte,
          estimate = tidyselect::ends_with("_string")
        ) %>%
        dplyr::mutate(
          performance_measure = paste(.data$analyte, "PPA"),
        ),
      npa %>%
        dplyr::select(
          .data$analyte,
          estimate = tidyselect::ends_with("_string")
        ) %>%
        dplyr::mutate(
          performance_measure = paste(.data$analyte, "NPA"),
        )
    ) %>%
    # TODO: It would be nice to make sure the sort order matches that used
    # in the panel or the evaluation.
    dplyr::arrange(.data$analyte) %>%
    dplyr::select(
      .data$performance_measure,
      .data$estimate
    )
  list(
    ppa = ppa,
    npa = npa,
    summary = summary
  )
}
