#' Positive Likelihood Ratio
#'
#' @param sensitivity_estimate The estimated sensitivity or PPA with full
#' precision (do not round this number before passing it to this function).
#' @param specificity_estimate The estimated specificity or NPA with full
#' precision (do not round this number before passing it to this function).
#' @param digits The number of digits to round to. Defaults to \code{0.1}.
#'
#' @return A positive Likelihood Ratio (LR+)
#' @export
#'
#' @examples
#' lr_pos(
#'   sensitivity_estimate = 30/30,
#'   specificity_estimate = 78/80,
#'   digits = 0.01
#' )
lr_pos <- function(sensitivity_estimate, specificity_estimate, digits = 0.1) {
  (sensitivity_estimate / (1 - specificity_estimate)) %>%
    plyr::round_any(
      x = .,
      accuracy = digits,
      f = round
    )
}

#' Negative Likelihood Ratio
#'
#' @param sensitivity_estimate The estimated sensitivity or PPA with full
#' precision (do not round this number before passing it to this function).
#' @param specificity_estimate The estimated specificity or NPA with full
#' precision (do not round this number before passing it to this function).
#' @param digits The number of digits to round to. Defaults to \code{0.1}.
#'
#' @return A negative Likelihood Ratio (LR-)
#' @export
#'
#' @examples
#' lr_neg(
#'   sensitivity_estimate = 30/30,
#'   specificity_estimate = 78/80,
#'   digits = 0.01
#' )
lr_neg <- function(sensitivity_estimate, specificity_estimate, digits = 0.1) {
  ((1 - sensitivity_estimate) / specificity_estimate) %>%
    plyr::round_any(
      x = .,
      accuracy = digits,
      f = round
    )
}
