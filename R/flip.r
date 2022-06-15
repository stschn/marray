#' @title Array flip
#' @description Reverse the order of elements in an array along the given axes.
#'
#' @param a An array.
#' @param axis Axis or axes along which to flip over.
#'
#' @details This function corresponds to \code{flip()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.flip.html}{see}).
#' Flipping along an axis can be exemplified with a matrix. If the order of the elements along the first dimension (row) is to be reversed,
#' it is helpful to imagine a horizontal axis (from left to right) in the middle of the matrix where flipping takes place. The first row
#' becomes the last, the second row the second last and so on until the last row becomes the first. The same applies for reversing the order
#' of the elements along the second dimension (column), with the distinction that the flipping axis is a vertical axis (from top to bottom)
#' in the middle of the matrix.
#'
#' \code{flipud} flips an array vertically, so along axis = 1. \code{fliplr} flips an array horizontally, so along axis = 2.
#'
#' @return The reversed array \code{a} along axes.
#'
#' @export
flip <- function(a, axis = 1L) {
  d <- DIM(a)
  nd <- length(d)
  axis[which((axis < 0L) | (axis > nd))] <- nd
  ds <- lapply(d, seq_len)
  ds[axis] <- lapply(ds[axis], rev)
  do.call('[', c(list(a), ds))
}

#' @rdname flip
#' @export
flipud <- function(a) {
  flip(atleast_1d(a), axis = 1L)
}

#' @rdname flip
#' @export
fliplr <- function(a) {
  flip(atleast_2d(a), axis = 2L)
}
