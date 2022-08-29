#' @title Array repetition
#' @description Construct an array by repeating \code{a} the number of times given by \code{reps}.
#'
#' @param a The input array.
#' @param reps Number of repetitions of \code{a} along each axis. If \code{reps} has length \code{d}, the result will have dimension of \code{max(d, ndim(a))}.
#'
#' @details This function corresponds to \code{tile()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.tile.html}{see}).
#'
#' @return The tiled output array.
#'
#' @seealso \code{\link{marepeat}}.
#'
#' @examples
#' a <- marray(c(0, 1, 2))
#' tile(a, 3)
#' tile(a, c(2, 2))
#' tile(a, c(2, 1, 2))
#'
#' a <- marray(1:4, dim = c(2, 2))
#' tile(a, 2)
#' tile(a, c(2, 1))
#'
#' @export
tile <- function(a, reps) {
  reps_len <- length(reps)
  a <- ndmin(a, n = reps_len, axis = 1L)
  if (all(reps == 1L)) return(a)

  d <- DIM(a)
  nd <- length(d)

  if (reps_len < nd)
    reps <- c(rep(1L, nd - reps_len), reps)
  newdim <- d * reps

  n <- nsize(a)
  if (n > 0)
    for (i in seq_along(d)) {
      nrep <- reps[i]
      if (nrep != 1) {
        remaining_len <- as.integer(nsize(a) / n)
        a <- reshape.array(a, dim = c(remaining_len, n))
        arys <- lapply(seq_len(nrep), function(i) a )
        a <- mabind(arys)
      }
      n <- floor(n / d[i])
    }
  reshape.array(a, dim = newdim)
}

#' @title Array repetition
#' @description Repeat elements of an array.
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
