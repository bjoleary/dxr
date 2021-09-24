#' Calculate an agreement proportion
#'
#' @param true_calls The number of correct calls (true positives or true
#' negatives).
#' @param false_calls The number of incorrect calls (false positives or false
#' negatives).
#' @param digits The number of digits to round to. Defaults to \code{0.1}.
#' @param interval The confidence interval to calculate. Defaults to
#' \code{0.95} for 95%.
#'
#' @return A formatted agreement proportion.
#' @export
#'
#' @examples
#' agreement(100, 0)
#' agreement(100, 0, 0.01, 0.99)
agreement <- function(true_calls, false_calls, digits = 0.1,
                      interval = 0.95) {
  ci <- confidence(true_calls = true_calls,
                   false_calls = false_calls,
                   interval = interval)
  lower_percent <- format_percent(ci$lower, digits = digits)
  upper_percent <- format_percent(ci$upper, digits = digits)
  confidence_interval <-
    paste0(
      "(", format_percent(interval, digits = 1), " CI: ",
      lower_percent, "; ", upper_percent, ")"
    )
  percent <-
    format_percent(
      percent = true_calls / (true_calls + false_calls),
      digits = digits
    )
  ratio <- paste0(true_calls, "/", true_calls + false_calls)
  list(
    estimate = true_calls / (true_calls + false_calls),
    estimate_percent = percent,
    ratio = ratio,
    lower = ci$lower,
    lower_percent = lower_percent,
    upper = ci$upper,
    upper_percent = upper_percent,
    confidence_interval = confidence_interval,
    string_two_line =
      paste0(
        percent, " (", ratio, ") ",
        "\n",
        "(", format_percent(interval, digits = 1), " CI: ",
        lower_percent, "; ", upper_percent, ")"
      ),
    string =
      paste(
        ratio, "=", percent, confidence_interval
      )
  )
}

#' Calculate an agreement proportion
#'
#' This is a wrapper around \code{agreement()} that permits use of a
#' numerator and denominator rather than true calls and false calls.
#'
#' @param numerator The numerator. Typically the number of true calls for a
#' proportion such as PPA or NPA.
#' @param denominator The total number of calls.
#' @param digits The number of digits to round to. Defaults to \code{0.1}.
#' @param interval The confidence interval to calculate. Defaults to
#' \code{0.95} for 95%.
#'
#' @return A formatted agreement proportion.
#' @export
#'
#' @examples
#' agreement_fraction(100, 100)
#' agreement_fraction(100, 100, 0.01, 0.99)
agreement_fraction <- function(numerator, denominator, digits = 0.1,
                               interval = 0.95) {
  false_calls <- denominator - numerator
  agreement(true_calls = numerator,
            false_calls = false_calls,
            digits = digits,
            interval = interval)
}

#' Calculate an agreement proportion for Sensitivity
#'
#' A wrapper around \code{agreement} that is tailored to PPA or sensitivity
#' calculations
#'
#' @param true_positives The number of correct calls.
#' @param false_negatives The number of incorrect calls.
#' @param digits The number of digits to round to. Defaults to \code{0.1}.
#' @param interval The confidence interval to calculate. Defaults to
#' \code{0.95} for 95%.
#'
#' @return A formatted agreement proportion.
#' @export
#'
#' @examples
#' sensitivity(100, 0)
#' sensitivity(100, 0, 0.01, 0.99)
sensitivity <- function(true_positives, false_negatives, digits = 0.1,
                        interval = 0.95) {
  agreement(true_calls = true_positives,
            false_calls = false_negatives,
            digits = digits,
            interval = interval)
}

#' Calculate an agreement proportion for Specificity
#'
#' A wrapper around \code{agreement} that is tailored to NPA or specificity
#' calculations
#'
#' @param true_negatives The number of correct calls.
#' @param false_positives The number of incorrect calls.
#' @param digits The number of digits to round to. Defaults to \code{0.1}.
#' @param interval The confidence interval to calculate. Defaults to
#' \code{0.95} for 95%.
#'
#' @return A formatted agreement proportion.
#' @export
#'
#' @examples
#' specificity(100, 0)
#' specificity(100, 0, 0.01, 0.99)
specificity <- function(true_negatives, false_positives, digits = 0.1,
                        interval = 0.95) {
  agreement(true_calls = true_negatives,
            false_calls = false_positives,
            digits = digits,
            interval = interval)
}

#' Calculate an agreement proportion for Sensitivity from a Fraction
#'
#' This is a wrapper around \code{agreement()} that permits use of a
#' numerator and denominator rather than true calls and false calls.
#'
#' @param numerator The numerator. The number of true positive calls.
#' @param denominator The total number of calls.
#' @param digits The number of digits to round to. Defaults to \code{0.1}.
#' @param interval The confidence interval to calculate. Defaults to
#' \code{0.95} for 95%.
#'
#' @return A formatted agreement proportion.
#' @export
#'
#' @examples
#' sensitivity_fraction(100, 100)
#' sensitivity_fraction(100, 100, 0.01, 0.99)
sensitivity_fraction <- function(numerator, denominator, digits = 0.1,
                                 interval = 0.95) {
  agreement_fraction(numerator = numerator,
                     denominator = denominator,
                     digits = 0.1,
                     interval = interval)
}

#' Calculate an agreement proportion for Specificity from a Fraction
#'
#' This is a wrapper around \code{agreement()} that permits use of a
#' numerator and denominator rather than true calls and false calls.
#'
#' @param numerator The numerator. The number of true negative calls.
#' @param denominator The total number of calls.
#' @param digits The number of digits to round to. Defaults to \code{0.1}.
#' @param interval The confidence interval to calculate. Defaults to
#' \code{0.95} for 95%.
#'
#' @return A formatted agreement proportion.
#' @export
#'
#' @examples
#' specificity_fraction(100, 100)
#' specificity_fraction(100, 100, 0.01, 0.99)
specificity_fraction <- function(numerator, denominator, digits = 0.1,
                                 interval = 0.95) {
  agreement_fraction(numerator = numerator,
                     denominator = denominator,
                     digits = 0.1,
                     interval = interval)
}
