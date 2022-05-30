#' @title Multidimensional array
#' @description
#'   \code{marray(data, ...)} creates a reshaped multidimensional array.\cr
#'   \code{as.marray(data, ...)} attempts to turn its argument into an array.\cr
#'
#' @param data The data to be reshaped to a multidimensional array.
#' @param dim The dimensions for the created array. If \code{dim} is not defined (default) and \code{data} already has dimensions, these will be applied.
#' @param dimnames Either \code{NULL} or the names of the dimensions. This must be a list with one component for each dimension, either \code{NULL} or a character vector of the length given by \code{dim} for that dimension.
#' @param order The order in which elements of data should be read during rearrangement.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This introduced n-dimensional array is an equivalent to \code{ndarray} class from NumPy (\url{https://numpy.org/}), a famous package in Python.
#'   Usually, an n-dimensional array is a multidimensional container consisting of bunches of bunches of bunches... of matrices.
#'   The first two dimensions define the matrix while the remaining dimensions define the corresponding bunches. For e.g., an 4x3x2 array has 2 bunches of each 4x3 matrix.
#'   An 6x4x3x2 array has 2 bunches, each of these two bunches has 3 bunches and each of these three bunches again contains a 6x4 matrix.
#'
#'   The behavior of \code{marray} is similar to that of ndarray from NumPy. R follows a column-major ordering (Fortran-style) during building up an array,
#'   wile Python respectively NumPy prefers row-major ordering (C-style) but offers both. For a comparison see \url{https://rstudio.github.io/reticulate/articles/arrays.html}.
#'
#' @return An array.
#'
#' @seealso \code{\link{array}}, \code{\link{dim}}, \code{\link{reshape.array}}.
#'
#' @examples
#' # Vector input with explicit dimensions
#' marray(1:24, dim = c(8, 3)) # 2D array with row-major ordering
#' marray(1:24, dim = c(8, 3), order = "F") # 2D array with column-major ordering
#' marray(1:24, dim = c(4, 3, 2)) # 3D array with row-major ordering
#' marray(1:24, dim = c(4, 3, 2), order = "F") # 3D array with column-major ordering
#'
#' # Different input types and applying the dimensions
#' v <- (1:24)
#' l <- list(x1 = 1:10, x2 = seq(10, 100, 10))
#' df <- data.frame(x1 = 1:6, x2 = seq(10, 60, 10), x3 = sample(letters, 6))
#' m <- matrix(1:24, nrow = 6)
#' a1 <- array(letters[1L:24L])
#' a3 <- array(v, dim = c(4, 3, 2))
#' a4 <- array(1:48, dim = c(4, 3, 2, 2))
#' data <- a3; data
#' a <- marray(data, order = "F"); a
#' @export
marray <- function(data, ...) {
  as.marray(data, ...)
}

#' @rdname marray
#' @export
as.marray <- function(data, ...) {
  UseMethod("as.marray")
}

#' @rdname marray
#' @export
as.marray.default <- function(data, dim = NULL, dimnames = NULL, order = c("C", "F")) {
  order <- match.arg(order)
  if (is.null(dim)) dim <- DIM(data)
  if (!is.array(data)) data <- array(data)
  data <- reshape.array(a = data, dim = dim, order = order)

  if (!is.null(dimnames)) { dimnames(data) <- dimnames }
  data
}

#' @rdname marray
#' @export
as.marray.data.frame <- function(data, dim = NULL, dimnames = NULL, order = c("C", "F")) {
  as.marray.default(as.matrix(data), dim = dim, dimnames = dimnames, order = order, numeric = numeric)
}

#' @rdname marray
#' @export
as.marray.list <- function(data, dim = NULL, dimnames = NULL, order = c("C", "F")) {
  as.marray.default(array(unlist(data)), dim = dim, dimnames = dimnames, order = order, numeric = numeric)
}
