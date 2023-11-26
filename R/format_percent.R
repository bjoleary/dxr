#' Format a percent
#'
#' Specifically, a percent based on a proportion that cannot exceed 1. Round
#' and format all values to the specified digits using the
#' \code{scales::percent} function, but replace outputs like 100.0%" and
#' "100.00%" with "100%".
#'
#' @param percent The percentage to be rounded, as a decimal (e.g. 0.95 for
#' 95%).
#' @param digits The number of digits to round to. Defaults to \code{0.1}.
#'
#' @return A formatted percent.
#' @export
#'
#' @examples
#' format_percent(0.95)
#' format_percent(0.9521231251, 0.0001)
format_percent <- function(percent, digits = 0.1) {
  value <- # nolint: object_usage_linter. ---- used in case_when
    scales::number(
        x = percent,
        accuracy = digits,
        scale = 100,
        prefix = "",
        suffix = "%",
        big.mark = " ",
        decimal.mark = ".",
        trim = TRUE
    )

  dplyr::case_when(
    stringr::str_detect(
      value,
      pattern = stringr::fixed("100.")
    ) ~ "100%",
    TRUE ~ value
  )
}
