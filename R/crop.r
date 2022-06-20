#' @title Array cropping
#' @description Crop array by selecting a slice and filling the rest with given values.
#'
#' @param a An array.
#' @param ... Indexing instructions with or without letters in form of \code{name = value} pairs. The names of the arguments specify the axis and the values its positions.
#' @param fill_value Value to fill the array beyond the slice defined by \code{...}.
#'
#' @return An array with the same shape as \code{a} including an original slice and with \code{fill_value} beyond the slice.
#'
#' @examples
#' a <- marray(1:24, dim = c(4, 3, 2))
#' crop(a, i = 1, j = 2:3)
#'
#' @export
crop <- function(a, ..., fill_value = NA) {
  out <- full(dim = DIM(a), fill_value = fill_value)
  slice(out, ...) <- slice(a, ...)
  out
}
