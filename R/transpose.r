#' @title Array transposition
#' @description Transpose an array.
#'
#' @param a An array.
#'
#' @return The array \code{a} with reversed or swapped dimensions.
#'
#' @seealso \code{\link{t}}, \code{\link{rearrange}}.
#'
#' @rdname transpositions
#' @export
t.array <- function(a) {
  aperm(a)
}

#' @param perm The permutation vector of the dimensions. The default \code{NULL} indicates to reverse the order of all dimensions.
#' @export
#' @rdname transpositions
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
  axis <- .standardize_axis(axis, nd)

  ds <- seq_along(d)
  s.call <- if (is.null(axis)) rev(ds) else ds[-axis]
  s.ans <- if (is.null(axis)) NULL else ds[axis]
  aperm(a, perm = c(s.call, s.ans))
}

#' @title Array axis swapping
#' @description Interchange two axes of an array.
#'
#' @param a An array.
#' @param axis1 First axis.
#' @param axis2 Second axis.
#'
#' @details This function corresponds to \code{swapaxes()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.swapaxes.html}{see}).
#'
#' @return The array \code{a} with swapped dimensions.
#'
#' @export
swapaxes <- function(a, axis1, axis2) {
  ds <- seq_along(DIM(a))
  nd <- length(ds)
  axis1 <- .standardize_axis(axis1, nd)
  axis2 <- .standardize_axis(axis2, nd)
  ds[c(axis1, axis2)] <- ds[c(axis2, axis1)]
  aperm(a, perm = ds)
}

#' @title Array axis moving
#' @description Move axes of an array to new positions while the other axes remain in their original order.
#'
#' @param a An array.
#' @param source An integerish vector indicating the original axes to move. These must be unique.
#' @param destination An integerish vector indicating the destination positions for each of the original axes. These must also be unique.
#'
#' @details This function corresponds to \code{moveaxis()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.moveaxis.html}{see}).
#'
#' @return The array \code{a} with moved axes.
#'
#' @examples
#' a <- marray(1:(3*4*5), dim = c(3, 4, 5), order = "F")
#' DIM(moveaxis(a, 1, 3))
#' DIM(moveaxis(a, 3, 1))
#' DIM(moveaxis(a, source = c(1, 2), destination = c(3, 1)))
#'
#' a <- marray(1:24, dim = c(4, 3, 2, 1))
#' DIM(moveaxis(a, source = c(1, 4), destination = c(3, 2)))
#'
#' @export
moveaxis <- function(a, source, destination) {
  ds <- seq_along(DIM(a))
  nd <- length(ds)
  source <- unique(source)
  destination <- unique(destination)
  source <- .standardize_axis(source, nd)
  destination <- .standardize_axis(destination, nd)
  stopifnot("source and destination must have the same number of elements." = length(source) == length(destination))

  newds <- setdiff(ds, source)
  source <- source[order(destination, decreasing = FALSE)]
  destination <- sort(destination)
  for (i in seq_along(source))
    newds <- append(newds, source[i], destination[i] - 1L)

  aperm(a, perm = newds)
}
