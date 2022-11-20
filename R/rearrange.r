#' @title Array flip
#' @description Reverse the order of elements in an array along the given axes.
#'
#' @param a An array.
#' @param axis Axis or axes along which to flip over.
#' @param drop For matrices and arrays. If \code{TRUE} the result is coerced to the lowest possible dimension. This only works for extracting elements, not for the replacement. See \code{\link[base]{drop}} for further details.
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
flip <- function(a, axis = 1L, drop = FALSE) {
  d <- DIM(a)
  nd <- length(d)
  axis <- .standardize_axis(axis, nd)
  ds <- lapply(d, seq_len)
  ds[axis] <- lapply(ds[axis], rev)
  do.call(`[`, c(list(a), ds, list(drop = drop)))
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

#' @title Array rotation
#' @description Rotate an array by 90 degrees in the plane specified by axes.
#'
#' @param a An array.
#' @param k Number of times the array is rotated by 90 degree. Positive numbers represent clockwise rotation, negative numbers counterclockwise rotation.
#' @param axes The array is rotated in the plane defined by the axes. Axes must be different.
#' @param drop For matrices and arrays. If \code{TRUE} the result is coerced to the lowest possible dimension. This only works for extracting elements, not for the replacement. See \code{\link[base]{drop}} for further details.
#'
#' @details This function corresponds to \code{rot90()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.rot90.html}{see}).
#' @return A rotated view of \code{a}.
#'
#' @export
rot90 <- function(a, k = 1L, axes = c(1L, 2L), drop = FALSE) {
  stopifnot("a must be at least a 2-dimensional array." = ndim(a) >= 2L,
            "axes must consist of two values to span the plane the array is rotated." = length(axes) == 2L)
  d <- DIM(a)
  nd <- length(d)
  axes <- .standardize_axis(axes, nd)
  # shape of the output: d[axes] <- d[rev(axes)]
  perm <- seq_len(ndim(a))
  perm[axes] <- perm[rev(axes)]

  # clockwise rotation
  if (k > 0L) {
    for (i in seq_len(k)) {
      a <- transpose(flip(a, min(axes), drop = drop), perm = perm)
    }
  }
  # counterclockwise rotation
  else {
    for (i in seq_len(abs(k))) {
      a <- flip(transpose(a, perm = perm), min(axes), drop = drop)
    }
  }
  a
}

#' @title Array circulation
#' @description Roll array elements along a given axis.
#'
#' @param a An array.
#' @param shift The number of places by which elements are shifted. If a tuple, then axis must be a tuple of the same size,
#'   and each of the given axes is shifted by the corresponding number. If a scalar while axis is a tuple of ints, then the same
#'   value is used for all given axes. A positive number indicates a shift to the right, a negative number to the left.
#' @param axis The axis or axes along \code{a} which elements are shifted.
#'   By default, the array is flattened before shifting, after which the original shape is restored.
#' @param order The order in which elements of data should be read during flattening.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds to \code{roll()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.roll.html}{see}).
#' Shift array circularly, that is, elements that roll beyond the last position are re-introduced at the first.
#'
#' @return An array with the same shape as \code{a} but with shifted elements along given axis.
#'
#' @examples
#' a <- marray(0:9)
#' # roll forward
#' roll(a, shift = 2)
#' # roll backward
#' roll(a, shift = -2)
#' a <- marray(a, dim = c(2, 5))
#' roll(a, shift = 1)
#' roll(a, shift = -1)
#' roll(a, shift = 1, axis = 1)
#' roll(a, shift = -1, axis = 1)
#'
#' @export
roll <- function(a, shift, axis = NULL, order = c("C", "F")) {
  if (any(shift == 0)) return(a)
  if (is.null(axis))
    return(marray(roll(flatten(a, order = order), shift, axis = 1L), dim = DIM(a)))
  else {
    a <- .standardize_array(a)
    axis <- .standardize_axis(axis, ndim(a) -> nd)

    if (length(shift) == 1L) shift <- rep(shift, length(axis))
    if (length(shift) > 1L)
      stopifnot("shift and axis must be of the same length." = length(shift) == length(axis))

    d <- DIM(a)
    ds <- lapply(d, seq)
    sr <- which(shift > 0)
    sl <- which(shift < 0)
    dr <- d[axis[sr]]
    dl <- d[axis[sl]]
    ds[axis[sr]] <- mapply(.shift_right, dr, shift[sr], SIMPLIFY = FALSE)
    ds[axis[sl]] <- mapply(.shift_left, dl, shift[sl], SIMPLIFY = FALSE)

    # shifts <- rep(0L, nd)
    # shifts[axis] <- shift
    #
    # d <- DIM(a)
    # arr_idx <- vector(mode = "list", length = nd)
    #
    # for (i in seq_along(arr_idx)) {
    #   if (shifts[i] > 0L) # shift right
    #     arr_idx[[i]] <- c(seq(d[i] - shifts[i] + 1L, d[i]), seq(1L, d[i] - shifts[i]))
    #   if (shifts[i] < 0L) # shift left
    #     arr_idx[[i]] <- c(seq(-shifts[i] + 1L, d[i]), seq(1L, -shifts[i]))
    # }

    slice(a, ds)
  }
}

.shift_right <- function(d, s) { c(seq(d - s + 1, d), seq(1L, d - s)) }
.shift_left <- function(d, s) { c(seq(-s + 1, d), seq(1L, -s)) }

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
