#' @title Array to matrix
#' @description Shrinks an array by combining the respective first two dimensions of the array by columns or rows.
#'
#' @param a An array.
#' @param order The order in which elements of data should be read during combination.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @return A two-dimensional array with combined matrices of the first two dimensions of \code{x}.
#'
#' @export
mamatrix <- function(a, order = c("C", "F")) {
  stopifnot("a must be at least a 2-dimensional array." = ndim(a) >= 2L)
  order <- match.arg(order)
  if (order == "C")
    apply(a, 2L, base::identity) # rbind()
  else
    t(apply(a, 1L, base::identity)) # cbind()
}
