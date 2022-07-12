#' @title Array repetition
#'
#' @param a The input array.
#' @param repeats An integerish vector or list of integerish vectors indicating the number of repetitions for each element along \code{axis}.
#' @param axis The axis or axes along which to repeat values. Per default, all axes are addressed.
#'
#' @details This function corresponds partially to \code{repeat()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.repeat.html}{see}) but extends its functionality to all axes of \code{a}.
#'
#' @return An output array which has the same shape as \code{a}, except along \code{axis}.
#'
#' @seealso \code{\link{tile}}.
#'
#' @examples
#' a <- marray(seq_len(4*4*2), dim = c(4, 4, 2))
#' marepeat(a, repeats = 2, axis = 1)
#' marepeat(a, repeats = c(2, 2, 3, 1), axis = 1)
#' marepeat(a, repeats = c(2, 2, 1, 1), axis = c(1, 2))
#' marepeat(a, repeats = c(2, 2, 1, 1), axis = 1) |> marepeat(repeats = c(2, 2, 1, 1), axis = 2)
#' marepeat(a, repeats = list(i = c(2, 1, 2, 1), j = c(1, 2, 3, 2)), axis = c(1, 2))
#'
#' @export
marepeat <- function(a, repeats, axis = NULL) {
  d <- DIM(a)
  ds <- lapply(d, seq_len)
  if (is.null(axis)) axis <- seq_along(d)
  axis <- .standardize_axis(axis, length(d))

  if (length(unlist(repeats)) == 1L) {
    ds[axis] <- lapply(mapply(rep, ds[axis], repeats, SIMPLIFY = FALSE), sort)
    return(slice(a, ds))
  }

  if (!is.list(repeats)) {
    if (any(length(repeats) != d[axis]))
      stop("number of repetitions must be equal to length of each axis.", call. = FALSE)
    ds[axis] <- lapply(seq_along(axis), function(i) unlist(mapply(rep, seq_len(d[axis[i]]), repeats, SIMPLIFY = FALSE)))
    return(slice(a, ds))
  } else {
    stopifnot("length of repeats must be equal to number of axis" = length(repeats) == length(axis))
    ds[axis] <- lapply(seq_along(axis), function(i) unlist(mapply(rep, seq_len(d[axis[i]]), repeats[[i]], SIMPLIFY = FALSE)))
    return(slice(a, ds))
  }
}
