#' Build Panel Sheet
#'
#' A function to build an excel file with fill-in-the-blank cells for common
#' information needed about a sample panel or ground-truth.
#'
#' @param panel_name The name of the panel being used for the evaluation. Must
#'   be a character string (not a multi-level vector) and must not be \code{NA}.
#' @param panel_description An optional sentence or paragraph describing the
#'   panel. Defaults to \code{NULL}.If provided, it must be a string rather than
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
#' @param semiquantitative_outcomes The valid semi-quantitative outcomes
#'   associated with the panel. Defaults to \code{NULL}, indicating that no
#'   semi-quantitative ground truth has been established for the sample panel.
#'   If titers or other information are available, the relevant levels must be
#'   provided as a character vector. For example: \code{c("0", "100", "400",
#'   "1600", "6400")} (where \code{0} corresponds to a sample with a negative
#'   qualitative result).
#' @param quantitative_units If quantitative outcomes have been established for
#'   the panel, this is a character string describing the units of those
#'   quantitative results. Defaults to \code{NULL}, indicating that no
#'   quantitative ground truth has been established for the sample panel. If the
#'   results are unit-less, \code{"Unit-less"} can be used.
#'
#' @return Returns \code{TRUE} if a sample panel sheet is created successfully.
#' @export
#'
#' @examples
#' build_panel_sheet(
#'   panel_name = "Example Panel",
#'   panel_description = "An example panel to illustrate use of the function.",
#'   n_samples = 110L,
#'   sample_groups = c("Positive", "Negative"),
#'   sample_matrices = c("Serum", "Plasma"),
#'   analytes = c("IgM", "IgG", "Pan-Ig"),
#'   targets = c("Spike", "Nucleocapsid"),
#'   qualitative_outcomes = c("Positive", "Negative"),
#'   semiquantitative_outcomes = c("0", "100", "400", "1600", "6400"),
#'   quantitative_units = NULL
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
  semiquantitative_outcomes = NA_character_,
  quantitative_units = NA_character_
) {
  # panel_name = "Panel 1"
  # panel_description = NA_character_
  # n_samples = 110
  # sample_groups = c("Positive", "Negative", "HIV+")
  # sample_matrices = c("Serum", "EDTA Plasma")
  # analytes = c("IgM", "IgG", "Pan-Ig")
  # targets = c("Spike")
  # qualitative_outcomes = c("Positive", "Negative")
  # semiquantitative_outcomes = c("0", "100", "400", "1600", "6400")
  # quantitative_units = NA_character_

  # Check inputs ---------------------------------------------------------------
  # panel_name must be a character string, not a vector
  if (
    !all(
      is.character(panel_name),
      length(panel_name) == 1,
      !is.na(panel_name)
    )
  ) {
    stop("The panel_name parameter must be a character string (not a vector).")
  }
  # panel description must be a character string (not a vector) or NULL
  if (!all(is.character(panel_description), length(panel_description) == 1)) {
    stop(
      "The panel_description parameter must be a character string (not a ",
      "vector) or it must be NA."
    )
  }
  # n_samples must be numeric, must convert to integer, must be greater than 0
  if (!is.numeric(n_samples)) {
    stop(
      "The n_samples parameter must be numeric."
    )
  } else {
    if (!is.integer(n_samples)) {
      n_samples <- as.integer(n_samples)
      warning(
        "Converting n_samples parameter to an intenger: ", n_samples, "."
      )
    }
    if (any(n_samples <= 0, is.na(n_samples))) {
      stop(
        "The n_samples parameter must be greater than zero."
      )
    }
  }
  # sample_groups is a character vector with at least one level
  if (
    any(
      !is.vector(sample_groups, mode = "character"),
      is.na(sample_groups)
      )
    ) {
    stop(
      "The sample_groups parameter must be a character vector and must ",
      "not be NA."
    )
  }
  # sample_matrices is a character vector with at least one level
  if (
    any(
      !is.vector(sample_matrices, mode = "character"),
      is.na(sample_matrices)
    )
  ) {
    stop(
      "The sample_matrices parameter must be a character vector and must ",
      "not be NA."
    )
  }
  # analytes is a character vector with at least one level
  if (
    any(
      !is.vector(analytes, mode = "character"),
      is.na(analytes)
      )
    ) {
    stop(
      "The analytes parameter must be a character vector and must ",
      "not be NA."
    )
  }
  # targets is a character vector with at least one level
  if (
    any(
      !is.vector(targets, mode = "character"),
      is.na(targets)
    )
  ) {
    stop(
      "The targets parameter must be a character vector and must ",
      "not be NA."
    )
  }
  # qualitative_outcomes is a character vector with at least one level
  if (
    any(
      !is.vector(qualitative_outcomes, mode = "character"),
      is.na(qualitative_outcomes)
    )
  ) {
    stop(
      "The qualitative_outcomes parameter must be a character vector and must ",
      "not be NA."
    )
  }
  # semiquantitative_outcomes must be a character vector or NA
  if (!is.vector(semiquantitative_outcomes, mode = "character")) {
    if (!is.na(semiquantitative_outcomes)) {
      stop(
        "The semiquantitative_outcomes parameter must be a character vector, ",
        "or it must be NA."
      )
    }
  }
  # quantitative_units must be a character string (not a vector) or NA
  if (!all(is.character(quantitative_units), length(quantitative_units) == 1)) {
      stop(
        "The quantitative_units parameter must be a character string (not a ",
        "vector) or it must be NA."
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
    semiquantitative_outcomes = semiquantitative_outcomes,
    quantitative_units = quantitative_units
  )

  # Write results? -------------------------------------------------------------

  # Finish
  TRUE
}
