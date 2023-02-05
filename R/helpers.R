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
  tidyr::expand_grid(
    analytes,
    qualitative_outcomes
  ) %>%
  tidyr::unite(col = "outcome", sep = " ") %>%
    # At this point, we have a single column tibble, with the column named outcome
    # that is a character vector of analytes and outcomes, such as
    # "IgM Positive", "IgG Positive", "IgM Negative", "IgG Negative"
    # Now that vector is a single column tibble, with the column named outcome
    dplyr::mutate(
      analytes =
        stringr::str_extract(
          string = .data$outcome,
          pattern = stringr::regex(paste(analytes, collapse = "|"))
        )
    ) %>%
    tidyr::pivot_longer(
      cols = "analytes"
    ) %>%
    # I'm honestly not sure we needed to do that, but this function seems to be
    # working, so...
    tidyr::pivot_wider(
      names_from = "value",
      values_from = "outcome",
      values_fn = list
    ) %>%
  # Okay, now we have a tibble with a name column, which just has "analyte" in
  # it, and a column for each analyte. Each analyte column contains a list,
  # like c("IgM Positive", "IgM Negative") in an IgM column. Let's unnest those.
    tidyr::unnest(
      cols = tidyselect::all_of(analytes)
    ) %>%
  # Alright, now we have a row for each outcome type. Let's get rid of the
  # name column.
    dplyr::select(
      -"name"
    ) %>%
    # Now cross all the values in the table, expanding their permutations to
    # include things like "IgM Positive, IgG Negative":
    tidyr::expand_grid(!!!.) %>%
    # Paste all the columns together
    tidyr::unite(col = "outcomes", sep = ", ") %>%
    # Pull out the result to a character vector
    dplyr::pull("outcomes") %>%
    # Cool. Now we have a character vector with all the possible outcomes
    # for each sample, including all analytes.
    # Replace the words positive and negative with symbols
    replace_pos_neg()
}

#' Lengthen
#'
#' Pad a vector to the desired length by adding \code{NA}.
#'
#' @param vec The vector
#' @param desired_length The length of the final vector
#'
#' @return A the input vector with \code{NA} appended until it reaches
#'   \code{desired_length}.
#'
lengthen <- function(vec, desired_length) {
  c(
    vec,
    rep(NA, desired_length - length(vec))
  )
}

#' Validation String
#'
#' Build a string describing where the valid values are in Excel. Do this by
#' extracting the column number of \code{column_vector} in
#' \code{input_validation} and converting it to a letter per Excel's column
#' naming convention. Then use the length of the vector used to form that column
#' to set the relevant rows.
#'
#' @param input_validation The table used to validate inputs in
#'   \code{panel_sheet_excel_method()} and
#'   \code{evaluation_sheet_excel_method()}.
#' @param column_vector The vector used to create the relevant column of
#'   \code{input_validation}.
#'
#' @return A string describing the location of the list of valid inputs in the
#'   excel file.
#'
validation_string <- function(input_validation, column_vector) {
  # we get an sprintf warning here
  column_letter <-
    LETTERS[[
      which(
        colnames(input_validation) ==
          as.character(substitute(column_vector))
      )
    ]]
  paste0(
    "\'input_validation\'!$",
    column_letter,
    "$2:$",
    column_letter,
    "$",
    length(column_vector) + 1
  )
}
