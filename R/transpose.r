#' @title Array transposition
#' @description Transpose an array.
#'
#' @param a An array.
#' @param perm The permutation vector of the dimensions. The default \code{NULL} indicates to reverse the order of all dimensions.
#'
#' @return The array \code{a} with swapped dimensions.
#'
#' @seealso \code{\link{t}}, \code{\link{rearrange}}.
#'
#' @export
transpose <- function(a, perm = NULL) {
  aperm(a, perm = perm)
}

#' @title Array rearrangement
#' @description Rearrange an array.
#'
#' @param a An array.
#' @param axis The axis or axes along \code{a} will be read into the new shape. The default \code{NULL} indicates the reverse order of all dimensions.
#'
#' @details Rearrangement of an array is equal to permutation its dimensions (axes). The given \code{axis} are the dimensions along the data of \code{a} are read.
#'   The remaining axes span the dimension space where the data are read into, including \code{axis} at the last position of the entire dimension space (shape).
#'
#' @return The array \code{a} with swapped dimensions.
#'
#' @seealso \code{\link{transpose}}.
#'
#' @export
rearrange <- function(a, axis = NULL) {
  d <- DIM(a)

  nd <- length(d)
  axis[which((axis < 0L) | (axis > nd))] <- nd

  ds <- seq_along(d)
  s.call <- if (is.null(axis)) rev(ds) else ds[-axis]
  s.ans <- if (is.null(axis)) NULL else ds[axis]
  aperm(a, perm = c(s.call, s.ans))
}
