#' Determine if a subgroup of negatives is cross-reactive
#'
#' This is a wrapper around \code{proportion_difference()}.
#'
#' @param true_calls_overall The number of true calls (true negatives) in all
#'   samples tested.
#' @param false_calls_overall The number of false calls (false positives) in all
#'   samples tested.
#' @param true_calls_subgroup The number of true calls (true negatives) in the
#'   subgroup of interest (e.g. HIV+).
#' @param false_calls_subgroup The number of false calls (false positives) in
#'   the subgroup of interest (e.g. HIV+).
#' @param digits The number of digits to round to. Defaults to \code{0.1}.
#' @param interval The confidence interval to calculate. Defaults to \code{0.95}
#'   for 95%.
#'
#' @return A list containing the estimated difference, the confidence interval,
#' a boolean attribute indicating whether or not a difference was detected at
#' the specified confidence (\code{difference_detected}), and various strings
#' and formatted percents that can be used to convey the information.
#' @export
#'
#' @examples
#' cross_reactive_subgroup(
#'   true_calls_overall = 79,
#'   false_calls_overall = 1,
#'   true_calls_subgroup = 9,
#'   false_calls_subgroup = 1,
#'   digits = 0.1,
#'   interval = 0.95
#' )
cross_reactive_subgroup <- function(
  true_calls_overall,
  false_calls_overall,
  true_calls_subgroup,
  false_calls_subgroup,
  digits = 0.1,
  interval = 0.95
) {

  true_calls_anti_subgroup <- true_calls_overall - true_calls_subgroup
  false_calls_anti_subgroup <- false_calls_overall - false_calls_subgroup

  proportion_difference(
    true_calls_1 = true_calls_anti_subgroup,
    false_calls_1 = false_calls_anti_subgroup,
    true_calls_2 = true_calls_subgroup,
    false_calls_2 = false_calls_subgroup,
    interval = interval,
    digits = digits
  )
}
