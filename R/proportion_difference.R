#' Calculate the confidence interval of a difference in proportions (e.g.
#' sensitivity or specificity)
#'
#' Calculate a confidence interval for a difference in proportions, such as PPA
#' or NPA. Uses the recommended method as described in Altman chapter 6 (loc
#' 1249 in the Kindle edition) Here, the z-score is calculated using the
#' \code{stats::qnorm()} function.
#'
#' Also known as "Newcombe's method".
#'
#' @param true_calls_1 The number of correct calls (true positives or true
#' negatives) in the first proportion of interest.
#' @param false_calls_1 The number of incorrect calls (false positives or false
#' negatives) in the first proportion of interest.
#' @param true_calls_2 The number of correct calls (true positives or true
#' negatives) in the second proportion of interest.
#' @param false_calls_2  The number of incorrect calls (false positives or false
#' negatives in the second proportion of interest).
#' @param interval The confidence interval to calculate. Defaults to
#' \code{0.95} for 95%.
#' @param digits The number of digits to round to. Defaults to \code{0.1}.
#'
#' @return A list containing the estimated difference, the confidence interval,
#' a boolean attribute indicating whether or not a difference was detected at
#' the specified confidence (\code{difference_detected}), and various strings
#' and formatted percents that can be used to convey the information.
#' @export
#'
#' @examples
#' proportion_difference(
#'   true_calls_1 = 99,
#'   false_calls_1 = 1,
#'   true_calls_2 = 80,
#'   false_calls_2 = 1,
#'   interval = 0.95
#'   )
proportion_difference <- function(
  true_calls_1,
  false_calls_1,
  true_calls_2,
  false_calls_2,
  interval = 0.95,
  digits = 0.1
) {
  proportion_1 <- agreement(
    true_calls = true_calls_1,
    false_calls = false_calls_1,
    digits = Inf, # Perform intermediate calculations at full precision
    interval = interval
  )
  proportion_2 <- agreement(
    true_calls = true_calls_2,
    false_calls = false_calls_2,
    digits = Inf, # Perform intermediate calculations at full precision
    interval = interval
  )

  # Calculate the difference
  d <- proportion_1$estimate - proportion_2$estimate
  estimate_percent <- format_percent(
    percent = d,
    digits = digits
  )

  # Calculate the lower bound
  lower_bound <-
    d -
    sqrt(
      (proportion_1$estimate - proportion_1$lower)^2 +
        (proportion_2$upper - proportion_2$estimate)^2
    )
  lower_percent <- format_percent(
    lower_bound,
    digits = digits
  )

  # Calculate the upper bound
  upper_bound <-
    d +
    sqrt(
      (proportion_2$estimate - proportion_2$lower)^2 +
        (proportion_1$upper - proportion_1$estimate)^2
    )
  upper_percent <- format_percent(
    upper_bound,
    digits = digits
  )

  # Format the confidence interval
  confidence_interval <-
    paste0(
      "(", format_percent(interval, digits = 1), " CI: ",
      lower_percent, "; ", upper_percent, ")"
    )

  # Determine if a difference is detected
  # A difference is detected when the confidence interval does not contain
  # zero (e.g. both the lower and upper bounds are positive or both the lower
  # and upper bounds are negative)
  difference_detected <- dplyr::case_when(
    lower_bound <= 0 & upper_bound <= 0 ~ TRUE,
    lower_bound >= 0 & upper_bound >= 0 ~ TRUE,
    # Otherwise, return FALSE
    TRUE ~ FALSE
  )
  difference_detected_string <-
    dplyr::case_when(
      difference_detected == TRUE ~
        paste0(
          "A difference of ",
          estimate_percent,
          " ",
          confidence_interval,
          " was detected."
        ),
      # Otherwise, it is false
      TRUE ~
        "A difference was not detected."
    )

  # Return results
  list(
    estimate = d,
    estimate_percent = estimate_percent,
    lower = lower_bound,
    lower_percent = lower_percent,
    upper = upper_bound,
    upper_percent = upper_percent,
    difference_detected = difference_detected,
    difference_detected_string = difference_detected_string,
    confidence_interval = confidence_interval,
    string_two_line =
      paste0(
        estimate_percent,
        "\n",
        confidence_interval
      ),
    string =
      paste(
        estimate_percent, confidence_interval
      )
  )
}

#' Calculate the confidence interval of a difference in proportions (e.g.
#' sensitivity or specificity)
#'
#' This is a wrapper around \code{proportion_difference()} that permits use of a
#' numerator and denominator rather than true calls and false calls.
#'
#' @param numerator_1 The numerator of the first proportion of interest.
#' Typically the number of true calls for a proportion such as PPA or NPA.
#' @param denominator_1 The total number of calls for the first proportion of
#' interest.
#' @param numerator_2 The numerator of the second proportion of interest.
#' Typically the number of true calls for a proportion such as PPA or NPA.
#' @param denominator_2 The total number of calls for the second proportion of
#' interest.
#' @param digits The number of digits to round to. Defaults to \code{0.1}.
#' @param interval The confidence interval to calculate. Defaults to
#' \code{0.95} for 95%.
#'
#' @return A list containing the estimated difference, the confidence interval,
#' a boolean attribute indicating whether or not a difference was detected at
#' the specified confidence (\code{difference_detected}), and various strings
#' and formatted percents that can be used to convey the information.
#' @export
#'
#' @examples
#' proportion_difference_fraction(
#'    numerator_1 = 99,
#'    denominator_1 = 100,
#'    numerator_2 = 79,
#'    denominator_2 = 80,
#'    interval = 0.95
#' )
proportion_difference_fraction <- function(
  numerator_1,
  denominator_1,
  numerator_2,
  denominator_2,
  interval = 0.95,
  digits = 0.1
) {
  false_calls_1 <- denominator_1 - numerator_1
  false_calls_2 <- denominator_2 - numerator_2

  proportion_difference(
    true_calls_1 = numerator_1,
    false_calls_1 = false_calls_1,
    true_calls_2 = numerator_2,
    false_calls_2 = false_calls_2,
    interval = interval,
    digits = digits
  )
}
