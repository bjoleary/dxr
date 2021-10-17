#' Get Panel Metadata
#'
#' A helper function to convert panel metadata from a nested list tibble to a
#' list.
#'
#' @param panel_data The output of \code{build_panel_sheet()} or
#'   \code{read_panel()}.
#'
#' @return The panel metadata as a list.
#'
get_panel_metadata <- function(panel_data) {
  metadata <-
    panel_data$panel_metadata %>%
    tidyr::pivot_wider() %>%
    purrr::map(unlist) %>%
    as.list()
}

#' Get Evaluation Metadata
#'
#' A helper function to convert evaluation metadata from a nested list tibble to
#' a list.
#'
#' @param evaluation_data the output of \code{build_evaluation_sheet()} or
#'   \code{read_evaluation()}.
#'
#' @return The evaluation metadata as a list.
#'
get_evaluation_metadata <- function(evaluation_data) {
  metadata <-
    evaluation_data$evaluation_metadata %>%
    tidyr::pivot_wider() %>%
    purrr::map(unlist) %>%
    as.list()
}

#' Replace Positive and Negative
#'
#' A helper function to replace " Positive" with "+" and " Negative" with "-".
#'
#' @param string The strign to modify
#'
#' @return A string with words replaced by symbols.
#'
replace_pos_neg <- function(string) {
  stringr::str_replace_all(
    string = string,
    pattern = "\\sNegative",
    replacement = "-"
  ) %>%
    stringr::str_replace_all(
      string = .,
      pattern = "\\sPositive",
      replacement = "+"
    )
}

#' Crossed Outcomes
#'
#' Come up with all the permutations of qualitative outcomes for each sample,
#' including all analytes.
#'
#' @param analytes The analytes to include.
#' @param qualitative_outcomes The qualitative outcomes to include.
#'
#' @return A vector of all permutations of analytes and qualitative outcomes.
#'
#' @examples
#' dxr:::crossed_outcomes(c("IgM", "IgG"), c("Positive", "Negative"))
#'
crossed_outcomes <- function(analytes, qualitative_outcomes) {
  # Gosh. There has to be a simpler way to do this.
  # TODO: Take a look at simplifying this. But only after you write some good
  # unit tests, okay?
  # All individual analyte outcomes
  purrr::cross2(
    analytes,
    qualitative_outcomes
  ) %>%
    purrr::map_chr(
      paste,
      collapse = " "
    ) %>%
  # At this point, we have a character vector of analytes and outcomes, such as
  # "IgM Positive", "IgG Positive", "IgM Negative", "IgG Negative"
    tibble::enframe(
      name = NULL,
      value = "outcome"
    ) %>%
  # Now that vector is a single column tibble, with the colum named outcome
    dplyr::mutate(
      analyte =
        stringr::str_extract(
          string = .data$outcome,
          pattern = analytes
        )
    ) %>%
    # Now we've added an analyte column, extracting the analyte from the
    # outcome column
    tidyr::pivot_longer(
      cols = .data$analyte
    ) %>%
    # I'm honestly not sure we needed to do that, but this function seems to be
    # working, so...
    tidyr::pivot_wider(
      names_from = .data$value,
      values_from = .data$outcome,
      values_fn = list
    ) %>%
  # Okay, now we have a tibble with a name column, which just has "analyte" in
  # it, and a column for each analyte. Each analyte column contains a list,
  # like c("IgM Positive", "IgM Negative") in an IgM column. Let's unnest those.
    tidyr::unnest(
      cols = analytes
    ) %>%
  # Alright, now we have a row for each outcome type. Let's get rid of the
  # name column.
    dplyr::select(
      -.data$name
    ) %>%
    # Now cross all the values in the table, expanding their permutations to
    # include things like "IgM Positive, IgG Negative":
    purrr::cross() %>%
    purrr::map_chr(
      paste,
      collapse = ", "
    ) %>%
    # Cool. Now we have a character vector with all the possible outcomes
    # for each sample, including all analytes.
    # Replace the words positive and negative with symbols
    replace_pos_neg()
}