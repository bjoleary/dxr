#' Confidence
#'
#' Calculate a confidence interval for a proportion, such as PPA or NPA. Uses
#' the recommended method as described in Altman chapter 6 (loc 1179 in the
#' Kindle edition) and in CLSI EP12-A2 (2008). Note that the CLSI method may
#' round the z-score for a 95% CI to 1.96, leading to slight differences in
#' computed values. Here, the z-score is calculated using the
#' \code{stats::qnorm()} function.
#'
#' Also known as "Wilson's method".
#'
#' @param true_calls The number of correct calls (true positives or true
#' negatives).
#' @param false_calls The number of incorrect calls (false positives or false
#' negatives).
#' @param interval The confidence interval to calculate. Defaults to
#' \code{0.95} for 95%.
#'
#' @return A list including a \code{lower_bound} and an \code{upper_bound}.
#' @export
#'
#' @examples
#' confidence(100, 0, 0.95)
#' confidence(100, 0)
#' confidence(100, 0, 0.99)
#' confidence(99, 1)
confidence <- function(true_calls, false_calls, interval = 0.95) {
  z_score <- stats::qnorm(
    p = interval + (1 - interval) * 1 / 2
  )
  q_1 <- 2 * true_calls + z_score^2
  q_2 <- z_score *
    sqrt(z_score^2 + 4 * true_calls * false_calls / (true_calls + false_calls))
  q_3 <- 2 * (true_calls + false_calls + z_score^2)

  # In Altman, A = q_1, B = q_2, and C = q_3.

  lower_bound <- (q_1 - q_2) / q_3
  upper_bound <- (q_1 + q_2) / q_3

  list(
    lower_bound = lower_bound,
    upper_bound = upper_bound
  )
}
