#' @title Array rotation
#' @description Rotate an array by 90 degrees in the plane specified by axes.
#'
#' @param a An array.
#' @param k Number of times the array is rotated by 90 degree. Positive numbers represent clockwise rotation, negative numbers counterclockwise rotation.
#' @param axes The array is rotated in the plane defined by the axes. Axes must be different.
#'
#' @details This function corresponds to \code{rot90()} from NumPy.
#' @return A rotated view of \code{a}.
#'
#' @export
rot90 <- function(a, k = 1L, axes = c(1L, 2L)) {
  stopifnot("a must be at least a 2-dimensional array." = ndim(a) >= 2L,
            "axes must consist of two values to span the plane the array is rotated." = length(axes) == 2L)
  d <- DIM(a)
  nd <- length(d)
  axes[which((axes < 0L) | (axes > nd))] <- nd
  # shape of the output: d[axes] <- d[rev(axes)]
  perm <- seq_len(ndim(a))
  perm[axes] <- perm[rev(axes)]

  # clockwise rotation
  if (k > 0L) {
    for (i in seq_len(k)) {
      a <- transpose(flip(a, min(axes)), perm = perm)
    }
  }
  # counterclockwise rotation
  else {
    for (i in seq_len(abs(k))) {
      a <- flip(transpose(a, perm = perm), min(axes))
    }
  }
  a
}
