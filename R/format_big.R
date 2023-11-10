#' Format big numbers
#'
#' A wrapper around \code{format()}.
#'
#' @param x The number to format.
#'
#' @return \code{x}, formatted with a comma for the big mark. Scientific
#'   notation is disabled.
#' @export
#'
format_big <- function(x) {
  format(
    x = x,
    big.mark = ",",
    scientific = FALSE
  )
}
