#' @title Array circulation
#' @description Roll array elements along a given axis.
#'
#' @param a An array.
#' @param shift The number of places by which elements are shifted. If a tuple, then axis must be a tuple of the same size,
#'   and each of the given axes is shifted by the corresponding number. If a scalar while axis is a tuple of ints, then the same
#'   value is used for all given axes. A positive number indicates a shift to the right, a negative number to the left.
#' @param axis The axis or axes along \code{a} which elements are shifted.
#'   By default, the array is flattened before shifting, after which the original shape is restored.
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
roll <- function(a, shift, axis = NULL) {
  if (any(shift == 0)) return(a)
  if (is.null(axis))
    return(marray(roll(flatten(a), shift, axis = 1L), dim = DIM(a)))
  else {
    if (length(axis) > ndim(a))
      stop(sprintf("number of specified axis (%d) is greater than the number of dimensions (%d).", length(axis), ndim(a)))
    stopifnot("axis can not be zero or negative." = all(axis > 0L),
              "any axis can not be greater than the number of dimensions." = all(axis <= ndim(a)))
    if (length(shift) > 1L)
      stopifnot("shift and axis must be of the same length." = length(shift) == length(axis))

    shifts <- rep(0L, ndim(a))
    shifts[axis] <- shift

    d <- DIM(a)
    arr_idx <- vector(mode = "list", length = (ndim(a)))

    for (i in seq_along(arr_idx)) {
      if (shifts[i] > 0L) # shift right
        arr_idx[[i]] <- c(seq(d[i] - shifts[i] + 1L, d[i]), seq(1L, d[i] - shifts[i]))
      if (shifts[i] < 0L) # shift left
        arr_idx[[i]] <- c(seq(-shifts[i] + 1L, d[i]), seq(1L, -shifts[i]))
    }

    slice(a, arr_idx)
  }
}
