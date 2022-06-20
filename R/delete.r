#' @title Array deletion
#' @description Delete axis of an array.
#'
#' @param a An array.
#' @param axis The axis or axes to delete from \code{a}.
#' @param keep The direction data of \code{a} are read for the newly created array. \code{first} denotes a reading of the first n-elements and \code{last} of last n elements.
#' @param order The order in which elements of \code{x} should be read during recreation after deleting \code{axis}.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds partially to \code{delete()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.delete.html}{see}).
#'   The number of elements is tailored for the reshaped array. The argument \code{keep} determines the reading direction for tailoring.
#'
#' @return The array \code{a} with deleted axes.
#'
#' @examples
#' # original array
#' a <- marray(1:24, dim = c(4, 3, 2), order = "F")
#' # delete the first dimension with reading first n elements
#' delete(a)
#' # delete the first dimension with reading last n elements
#' delete(a, keep = "last")
#' # delete the axes one and two with reading first n elements
#' delete(a, axis = c(1L, 2L))
#'
#' @export
delete <- function(a, axis = 1L, keep = c("first", "last"), order = c("C", "F")) {
  keep <- match.arg(keep)
  order <- match.arg(order)
  d <- DIM(a)
  nd <- ndim(a)

  if (nd <= 1L) {
    dim(a) <- NULL
    attributes(a) <- NULL
    return(a)
  }

  axis <- .standardize_axis(axis, nd)
  keep_axis <- setdiff(seq_along(d), axis)
  d <- d[keep_axis]
  size <- prod(d)
  dim(a) <- NULL

  if (identical(keep, "first"))
    # Reading the first n elements
    a <- a[seq_len(size)]
  else
    # Reading the last n elements
    a <- a[length(a) - ((size - 1L):0L)] #a[seq.int(length(a) - size + 1L, length(a))]

  marray(a, dim = d, order = order)
}
