#' @title Array deletion
#' @description Delete parts of an array.
#'
#' @param a An array.
#' @param ... Indexing instructions with or without letters in form of \code{name = value} pairs. The names of the arguments specify the axis and the values its positions.
#' @param drop For matrices and arrays. If \code{TRUE} the result is coerced to the lowest possible dimension. This only works for extracting elements, not for the replacement. See \code{\link[base]{drop}} for further details.
#'
#' @details This function corresponds partially to \code{delete()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.delete.html}{see}).
#'
#' @return The array \code{a} with deleted positions per axis.
#'
#' @examples
#' # original array
#' a <- marray(1:24, dim = c(4, 3, 2), order = "F")
#' # delete nothing
#' delete(a)
#' # delete positions 1 and 2 of the first axis
#' delete(a, i = 1:2)
#' # delete second row and third column
#' delete(a, i = 2, j = 3)
#'
#' @export
delete <- function(a, ..., drop = FALSE) {
  if (missing(...)) return(a)
  ds <- lapply(DIM(a) -> d, seq)
  aidx <- .aindex(d, ...)

  keep <- vector(mode = "list", length = length(d))
  for (i in seq_along(ds)) {
    keep[[i]] <- setdiff(ds[[i]], aidx[[i]])
    if (!length(keep[[i]])) keep[[i]] <- ds[[i]]
  }
  slice(a, keep, drop = drop)
}

#' @rdname delete
#' @description Delete whole axis from an array.
#'
#' @param axis The axis or axes to delete from \code{a}.
#' @param keep The direction data of \code{a} are read for the newly created array. \code{first} denotes a reading of the first n-elements and \code{last} of last n elements.
#' @param order The order in which elements of \code{x} should be read during recreation after deleting \code{axis}.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details The number of elements is tailored for the reshaped array. The argument \code{keep} determines the reading direction for tailoring.
#'
#' @return The array \code{a} with deleted axes.
#'
#' @examples
#' # original array
#' a <- marray(1:24, dim = c(4, 3, 2), order = "F")
#' # delete the first dimension with reading first n elements
#' erase(a)
#' # delete the first dimension with reading last n elements
#' erase(a, keep = "last")
#' # delete the axes one and two with reading first n elements
#' erase(a, axis = c(1L, 2L))
#'
#' @export
erase <- function(a, axis = 1L, keep = c("first", "last"), order = c("C", "F")) {
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
