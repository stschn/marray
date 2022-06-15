#' @title Expand the shape of an array
#' @description Insert a new axis that will appear at the axis position in the expanded array shape.
#'
#' @param a An array.
#' @param axis Index position of the new axis in the expanded array. Negative numbers count from the back.
#'
#' @details This function corresponds to \code{expand_dims()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.expand_dims.html}{see}).
#' @return The expanded array \code{a} with new shape.
#'
#' @references Implementation credits go to \url{https://github.com/t-kalinowski/listarrays}.
#'
#' @export
expand_dims <- function(a, axis = -1L) {
  d <- DIM(a)
  nd <- length(d)
  naxis <- length(axis)

  wd <- axis
  neg <- wd < 0L
  if (any(neg))
    wd[neg] <- wd[neg] + nd + naxis + 1L

  if (min(wd) < 1L)
    stop("implicit additional dims for expansions with negative indexes are not supported.")

  if ((max_wd <- max(wd)) > nd + naxis) {
    # Implicitly pad on right
    wd <- unique(c(wd), (nd + 1L):max_wd)
    ndout <- max_wd
  } else
    ndout <- nd + naxis

  if (anyDuplicated(wd)) {
    wd <- unique(wd)
  }

  dims <- rep(1L, ndout)
  dims[-wd] <- d

  dim(a) <- dims
  a
}
