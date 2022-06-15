#' @title Array deletion
#' @description Delete axis of an array.
#'
#' @param a An array.
#' @param axis The axis or axes to delete from \code{a}.
#' @param order The order in which elements of \code{x} should be read during recreation after deleting \code{axis}.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds partially to \code{delete()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.delete.html}{see}).
#'   The number of elements is tailored for the reshaped array. The sign of \code{axis} determines the reading direction for the extraction
#'   of the elements from \code{a} for the newly created array. A positive sign indicates that the first n elements from \code{a} are used,
#'   a negative sign indicates that the last n elements will be used.
#'
#' @return The array \code{a} with deleted axes.
#'
#' @examples
#' # original array
#' a <- marray(1:24, dim = c(4, 3, 2), order = "F")
#' # delete the first dimension with reading elements start from first position
#' delete(a)
#' # delete the first dimension with reading elements start from last position
#' delete(a, axis = -1L)
#' # delete the axes one and two with reading elements start from first position
#' delete(a, axis = c(1L, 2L))
#'
#' @export
delete <- function(a, axis = 1L, order = c("C", "F")) {
  order <- match.arg(order)
  d <- DIM(a)
  nd <- ndim(a)

  if (nd <= 1L) {
    dim(a) <- NULL
    attributes(a) <- NULL
    return(a)
  }

  start_last <- any(axis < 0L)
  axis <- abs(axis)
  axis[which(axis > nd)] <- nd

  keep <- setdiff(seq_along(d), axis)
  d <- d[keep]
  size <- prod(d)
  dim(a) <- NULL
  if (start_last)
    # Reading the last n elements
    a <- a[length(a) - ((size - 1L):0L)]
  else
    # Reading the first n elements
    a <- a[seq_len(size)]

  marray(a, dim = d, order = order)
}
