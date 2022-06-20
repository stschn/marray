#' @title Array insertion
#' @description Insert an object into an array.
#'
#' @param a An array.
#' @param ... Any number of objects inserted into or appended to \code{a}.
#' @param axis The axis along which to insert the objects.
#' @param order The order according to which the respective elements of the objects are read during insertion or appending.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds partially to \code{insert()} and \code{append()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.insert.html}{see}).
#'
#' @return The array \code{a} with objects inserted or appended.
#'
#' @examples
#' # original array
#' a <- array(seq.int(2 * 3 * 4), dim = c(2, 3, 4))
#' # slice to be added to the second axis
#' b <- array(100L + seq.int(2 * 1 * 4), dim = c(2, 1, 4))
#' insert(a, b, axis = 2L)
#'
#' @export
insert <- function(a, ..., axis = -1L, order = c("C", "F")) {
  order <- match.arg(order)
  d <- DIM(a)
  nd <- ndim(a)
  axis <- .standardize_axis(axis, nd)

  x <- .dots(...)

  # Reshape x with the same dimension as a but replacing the axis dimension with 1
  if (nd > 1L) {
    d[axis] <- 1L
    x <- lapply(x, FUN = marray, dim = d, order = order)
  }
  # Just bind the arrays along axis
  mabind(append(x, list(a), 0L), axis = axis)
}
