#' Positive Predictive Value
#'
#' Calculate Positive Predictive Value (PPV) in a population based on
#' sensitivity, specificity, and prevalence estimates.
#'
#' @param sensitivity The estimate of Sensitivity (PPA)
#' @param specificity The estimate of Specificity (NPA)
#' @param prevalence  The estimate of the prevalence of the condition in the
#' population.
#'
#' @return The positive predictive value
#' @export
#'
#' @examples
#' ppv(0.95, 0.95, 0.05)
ppv <- function(sensitivity, specificity, prevalence) {
  se <- sensitivity
  sp <- specificity
  p <- prevalence
  (se * p) / ((se * p) + (1 - sp) * (1 - p))
}

#' Negative Predictive Value
#'
#' Calculate Negative Predictive Value (NPV) in a population based on
#' sensitivity, specificity, and prevalence estimates.
#'
#' @param sensitivity The estimate of Sensitivity (PPA)
#' @param specificity The estimate of Specificity (NPA)
#' @param prevalence  The estimate of the prevalence of the condition in the
#' population.
#'
#' @return The negative predictive value
#' @export
#'
#' @examples
#' npv(0.95, 0.95, 0.05)
npv <- function(sensitivity, specificity, prevalence) {
  se <- sensitivity
  sp <- specificity
  p <- prevalence
  (sp * (1 - p)) / ((1 - se) * p + sp * (1 - p))
}

#' Positive Predictive Value (with confidence interval)
#'
#' Calculate the positive predictive value (PPV) with a confidence interval
#' based on a sensitivity output from \code{agreement()},
#' \code{agreement_fraction()}, \code{sensitivity()}, or
#' \code{sensitivity_fraction()} and a specificity output from
#' \code{agreement()}, \code{agreement_fraction()}, \code{specificity()}, or
#' \code{specificity_fraction()}.
#'
#' @param sensitivity Output of a function such as \code{sensitivity()}.
#' @param specificity Output of a function such as \code{specificity()}.
#' @param prevalence  The estimate of the prevalence of the condition in the
#' population.
#' @param interval The confidence interval used in the sensitivity and
#' specificity calculations.
#'
#' @return The positive predictive value, with confidence.
#' @export
#'
#' @examples
#' ppa <- sensitivity(99, 0)
#' npa <- specificity(99, 0)
#' ppv_with_confidence(ppa, npa, 0.05, 0.95)
ppv_with_confidence <- function(sensitivity, specificity, prevalence,
                                interval) {
  estimate <- ppv(
    sensitivity = sensitivity$estimate,
    specificity = specificity$estimate,
    prevalence = prevalence
  )

  percent <- format_percent(estimate)

  lower <- ppv(
    sensitivity = sensitivity$lower,
    specificity = specificity$lower,
    prevalence = prevalence
  )

  lower_percent <- format_percent(lower)

  upper <- ppv(
    sensitivity = sensitivity$upper,
    specificity = specificity$upper,
    prevalence = prevalence
  )

  upper_percent <- format_percent(upper)

  confidence_interval <-
    paste0(
      "(",
      interval * 100,
      "% CI: ",
      lower_percent,
      "; ",
      upper_percent,
      ")"
    )

  list(
    estimate = estimate,
    estimate_percent = percent,
    lower = lower,
    lower_percent = lower_percent,
    upper = upper,
    upper_percent = upper_percent,
    confidence_interval = confidence_interval,
    string_two_line =
      paste0(
        percent,
        "\n",
        confidence_interval
      ),
    string =
      paste(
        percent, confidence_interval
      )
  )
}

#' Negative Predictive Value (with confidence interval)
#'
#' Calculate the negative predictive value (NPV) with a confidence interval
#' based on a sensitivity output from \code{agreement()},
#' \code{agreement_fraction()}, \code{sensitivity()}, or
#' \code{sensitivity_fraction()} and a specificity output from
#' \code{agreement()}, \code{agreement_fraction()}, \code{specificity()}, or
#' \code{specificity_fraction()}.
#'
#' @param sensitivity Output of a function such as \code{sensitivity()}.
#' @param specificity Output of a function such as \code{specificity()}.
#' @param prevalence  The estimate of the prevalence of the condition in the
#' population.
#' @param interval The confidence interval used in the sensitivity and
#' specificity calculations.
#'
#' @return The negative predictive value, with confidence.
#' @export
#'
#' @examples
#' ppa <- sensitivity(99, 0)
#' npa <- specificity(99, 0)
#' npv_with_confidence(ppa, npa, 0.05, 0.95)
npv_with_confidence <- function(sensitivity, specificity, prevalence,
                                interval) {
  estimate <- npv(
    sensitivity = sensitivity$estimate,
    specificity = specificity$estimate,
    prevalence = prevalence
  )

  percent <- format_percent(estimate)

  lower <- npv(
    sensitivity = sensitivity$lower,
    specificity = specificity$lower,
    prevalence = prevalence
  )

  lower_percent <- format_percent(lower)

  upper <- npv(
    sensitivity = sensitivity$upper,
    specificity = specificity$upper,
    prevalence = prevalence
  )

  upper_percent <- format_percent(upper)

  confidence_interval <-
    paste0(
      "(",
      interval * 100,
      "% CI: ",
      lower_percent,
      "; ",
      upper_percent,
      ")"
    )

  list(
    estimate = estimate,
    estimate_percent = percent,
    lower = lower,
    lower_percent = lower_percent,
    upper = upper,
    upper_percent = upper_percent,
    confidence_interval = confidence_interval,
    string_two_line =
      paste0(
        percent,
        "\n",
        confidence_interval
      ),
    string =
      paste(
        percent, confidence_interval
      )
  )
}
