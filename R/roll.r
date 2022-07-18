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
