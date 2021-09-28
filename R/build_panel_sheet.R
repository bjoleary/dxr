#' Build Panel Sheet
#'
#' A function to build an excel file with fill-in-the-blank cells for common
#' information needed about a sample panel or ground-truth.
#'
#' @param panel_name The name of the panel being used for the evaluation. Must
#'   be a character string (not a multi-level vector) and must not be \code{NA}.
#' @param panel_description An optional sentence or paragraph describing the
#'   panel. Defaults to \code{NA}. If provided, it must be a string rather than
#'   a multi-level vector.
#' @param n_samples The number of samples in the panel. Must be numeric and
#'   greater than 0. If not provided as an integer, it will be converted into
#'   one.
#' @param sample_groups The descriptive terms used to describe groups of
#'   samples, provided as a character vector. Defaults to \code{c("Positive",
#'   "Negative")} for evaluations of qualitative tests, but additional levels
#'   can be added for, for example, potentially cross-reactive negative
#'   subgroups. This must be a character vector with at least one level.
#' @param sample_matrices The matrix or matrices in which the samples are
#'   provided. For example, for a serology assay panel, this could be
#'   \code{"Serum"} for serum-only,  \code{"EDTA Plasma"} for plasma only, or
#'   \code{c("Serum", "EDTA Plasma")} for evaluations involving multiple sample
#'   types. This must be provided with at least one level (not \code{NA}).
#' @param analytes The analyte or analytes for which the panel has been
#'   characterized and truth has been established. For example, for a serology
#'   assay panel, this could be \code{"Pan-Ig"} or \code{c("IgM", "IgG",
#'   "Pan-Ig")}. This must be provided with at least one level (not \code{NA}).
#' @param targets The target or targets for which the panel has been
#'   characterized and truth has been established. For example, for a SARS-CoV-2
#'   assay, this could be \code{"Spike"} or \code{"RBD"}. Multiple targets can
#'   also be used, such as \code{c("Spike", "Nucleocapsid")}. This must be
#'   provided with at least one level (not \code{NA}).
#' @param qualitative_outcomes The valid qualitative outcomes associated with
#'   the panel. Typically, this should be \code{c("Positive", "Negative")}. Note
#'   that while the assay being evaluated may also provide an "Equivocal" result
#'   level, this level should not typically be included in the panel sheet,
#'   where ground truth is expected to have been determined for each sample
#'   present. This must be provided with at least one level (not \code{NA}).
#' @param qualitative_comparators The qualitative comparator methods used to
#'   determine qualitative ground truth for the panel. For example, if an
#'   FDA-authorized PCR assay is used to determine ground truth--positive or
#'   negative--for an evaluation of an antigen assay, this can be provided as
#'   \code{"FDA-authorized PCR assay"} (or, ideally, the specific assay used,
#'   sample matrix, etc.). If different methods were used to characterize
#'   different samples, provide each method as an element of a character vector.
#'   For example, \code{c("FDA-authorized PCR Assay 1", "FDA-authorized PCR
#'   Assay 2", "Both FDA-authorized PCR Assay 1 and 2")}.
#' @param semiquantitative_outcomes The valid semi-quantitative outcomes
#'   associated with the panel. Defaults to \code{NA}, indicating that no
#'   semi-quantitative ground truth has been established for the sample panel.
#'   If titers or other information are available, the relevant levels must be
#'   provided as a character vector. For example: \code{c("0", "100", "400",
#'   "1600", "6400")} (where \code{0} corresponds to a sample with a negative
#'   qualitative result).
#' @param quantitative_units If quantitative outcomes have been established for
#'   the panel, this is a character string describing the units of those
#'   quantitative results. Defaults to \code{NA}, indicating that no
#'   quantitative ground truth has been established for the sample panel. If the
#'   results are unit-less, \code{"Unit-less"} can be used.
#'
#' @return Returns \code{TRUE} if a sample panel sheet is created successfully.
#' @export
#'
#' @examples
#' build_panel_sheet(
#'   panel_name = "Example Panel",
#'   panel_description = NA_character_,
#'   n_samples = 110L,
#'   sample_groups = c("Positive", "Negative"),
#'   sample_matrices = c("Serum", "Plasma"),
#'   analytes = c("IgM", "IgG", "Pan-Ig"),
#'   targets = c("Spike", "Nucleocapsid"),
#'   qualitative_outcomes = c("Positive", "Negative"),
#'   qualitative_comparators =
#'     c(
#'       "CDC Spike Antigen Assay and NCI Implementation of MS RBD Assay",
#'       "Collected Pre-2020"
#'     ),
#'   semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
#'   quantitative_units = NA_character_
#' )
build_panel_sheet <- function(
  panel_name,
  panel_description = NA_character_,
  n_samples,
  sample_groups = c("Positive", "Negative"),
  sample_matrices,
  analytes,
  targets,
  qualitative_outcomes = c("Positive", "Negative"),
  qualitative_comparators,
  semiquantitative_outcomes = NA_character_,
  # TODO: add semiquantitative_comparator
  quantitative_units = NA_character_
  # TODO: add quantitative_comparator
) {
  # Check inputs ---------------------------------------------------------------
  stopifnot(
    # panel_name must be a character string, not a vector
    is.character(panel_name),
    length(panel_name) == 1,
    !is.na(panel_name),
    !is.null(panel_name),
    # panel description must be a character string (not a vector)
    is.character(panel_description),
    length(panel_description) == 1,
    !is.null(panel_description),
    # n_samples must be numeric, must convert to integer, must be greater than 0
    is.numeric(n_samples),
    n_samples > 0,
    !is.na(n_samples),
    # sample_groups is a character vector with at least one level
    is.vector(sample_groups, mode = "character"),
    !is.na(sample_groups),
    # sample_matrices is a character vector with at least one level
    is.vector(sample_matrices, mode = "character"),
    !is.na(sample_matrices),
    # analytes is a character vector with at least one level
    is.vector(analytes, mode = "character"),
    !is.na(analytes),
    # targets is a character vector with at least one level
    is.vector(targets, mode = "character"),
    !is.na(targets),
    # qualitative_outcomes is a character vector with at least one level
    is.vector(qualitative_outcomes, mode = "character"),
    !is.na(qualitative_outcomes),
    # qualitative_comparators is a character vector with at least one level
    is.vector(qualitative_comparators, mode = "character"),
    !is.na(qualitative_comparators),
    # semiquantitative_outcomes must be a character vector or NA
    any(
      is.vector(semiquantitative_outcomes, mode = "character"),
      is.na(semiquantitative_outcomes)
    ),
    # quantitative_units must be a character string (not a vector) or NA
    any(
      all(
        is.character(quantitative_units),
        length(quantitative_units) == 1
        ),
      is.na(quantitative_units)
    )
  )
  if (!is.integer(n_samples)) {
    n_samples <- as.integer(n_samples)
    warning(
      "Converting n_samples parameter to an intenger: ", n_samples, "."
    )
  }

  # Build sheet ----------------------------------------------------------------

  metadata <-
    list(
      panel_name = panel_name,
      panel_description = panel_description,
      n_samples = n_samples,
      sample_groups = sample_groups,
      sample_matrices = sample_matrices,
      analytes = analytes,
      targets = targets,
      qualitative_outcomes = qualitative_outcomes,
      qualitative_comparators = qualitative_comparators,
      semiquantitative_outcomes = semiquantitative_outcomes,
      quantitative_units = quantitative_units
    ) %>%
    tibble::enframe()
  padding <- nchar(as.character(n_samples))
  sample_id <-
    paste(
      snakecase::to_snake_case(panel_name),
      stringr::str_pad(
        string = as.character(seq(1, n_samples, 1)),
        width = nchar(as.character(n_samples)),
        side = c("left"),
        pad = "0"
      ),
      sep = "_"
    )
  panel_table <-
    tidyr::expand_grid(
      sample = sample_id,
      analyte = analytes,
      target = targets
    ) %>%
    dplyr::mutate(
      group = NA_character_,
      matrix = NA_character_,
      qualitative_result = NA_character_
    )

  # Add the semiquantitative results column if applicable
  if (!all(is.na(semiquantitative_outcomes))) {
    panel_table <-
      panel_table %>%
      dplyr::mutate(
        semiquantitative_result = NA_character_
      )
  }

  # Add the semiquantitative results column if applicable
  if (!all(is.na(quantitative_units))) {
    panel_table <-
      panel_table %>%
      dplyr::mutate(
        quantitative_result = NA_character_
      )
  }
  # Write results? -------------------------------------------------------------

  # Finish
  list(
    panel_metadata = metadata,
    panel_table = panel_table
  )
}
