#' @title Array creation
#' @description Create 2D identity matrix.
#'
#' @param n The number of rows.
#' @param m The number of columns, default \code{NULL}.
#' @param dimnames Either \code{NULL} or the names of the dimensions. This must be a list with one component for each dimension, either \code{NULL} or a character vector of length \code{n}.
#'
#' @details This function corresponds to \code{eye()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.eye.html}{see}).
#' @return An identity matrix with n rows and n or rather m columns.
#'
#' @export
eye <- function(n, m = NULL, dimnames = NULL) {
  if (is.null(m)) m <- n
  if (m < n) m <- n
  mat <- zeros(dim = c(n, m), dimnames = dimnames)
  diag(mat) <- 1
  mat
}

#' @title Array creation
#' @description Return the identity array, a square array with ones on the main diagonal.
#'
#' @param n Number of rows and columns in \emph{n x n} output.
#' @param dimnames Either \code{NULL} or the names of the dimensions. This must be a list with one component for each dimension, either \code{NULL} or a character vector of length \code{n}.
#'
#' @details This function corresponds to \code{identity()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.identity.html}{see}).
#' @return \emph{n x n} array with its main diagonal set to one, and all other elements to zero.
#'
#' @export
maidentity <- function(n, dimnames = NULL) {
  eye(n, dimnames = dimnames)
}

#' @title Array creation
#' @description Create Vandermonde matrix.
#'
#' @param data The data to be reshaped to a Vandermonde matrix.
#' @param n The number of columns of the resulting matrix. If n is not specified, a square array is returned.
#' @param dimnames Either \code{NULL} or the names of the dimensions. This must be a list with one component for each dimension, either \code{NULL} or a character vector of length \code{n}.
#'
#' @details This function corresponds to \code{vander()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.vander.html}{see}).
#' @return A Vandermonde matrix.
#'
#' @export
vander <- function(data, n = NULL, dimnames = NULL) {
  data <- flatten(data)
  if (is.null(n)) n <- length(data)
  marray(outer(data, seq(0, n - 1), "^"), dimnames = dimnames, order = "F")
}

#' @title Array creation
#' @description Create array filled with ones.
#'
#' @param dim Shape of the new array.
#' @param dimnames Either \code{NULL} or the names of the dimensions. This must be a list with one component for each dimension, either \code{NULL} or a character vector of the length given by \code{dim} for that dimension.
#'
#' @details This function corresponds to \code{ones()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.ones.html}{see}).
#' @return An array of ones with the given shape.
#'
#' @export
ones <- function(dim = NULL, dimnames = NULL) {
  marray(rep(1L, prod(dim)), dim = dim, dimnames = dimnames)
}

#' @rdname ones
#'
#' @param a An array whose shape is taken for the result array.
#'
#' @details This function corresponds to \code{ones_like()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.ones_like.html#numpy.ones_like}{see}).
#' @return An array of ones with the the same shape as \code{a}.
#'
#' @export
ones_like <- function(a) {
  ones(dim = DIM(a), dimnames = dimnames(a))
}

#' @title Array creation
#' @description Create array filled with zeros.
#'
#' @param dim Shape of the new array.
#' @param dimnames Either \code{NULL} or the names of the dimensions. This must be a list with one component for each dimension, either \code{NULL} or a character vector of the length given by \code{dim} for that dimension.
#'
#' @details This function corresponds to \code{zeros()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.zeros.html}{see}).
#' @return An array of zeros with the given shape.
#'
#' @export
zeros <- function(dim = NULL, dimnames = NULL) {
  marray(rep(0L, prod(dim)), dim = dim, dimnames = dimnames)
}

#' @rdname zeros
#'
#' @param a An array whose shape is taken for the result array.
#'
#' @details This function corresponds to \code{zeros_like()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.zeros_like.html#numpy.zeros_like}{see}).
#' @return An array of zeros with the the same shape as \code{a}.
#'
#' @export
zeros_like <- function(a) {
  zeros(dim = DIM(a), dimnames = dimnames(a))
}

#' @title Array creation
#' @description Create array filled with NA.
#'
#' @param dim Shape of the new array.
#' @param dimnames Either \code{NULL} or the names of the dimensions. This must be a list with one component for each dimension, either \code{NULL} or a character vector of the length given by \code{dim} for that dimension.
#'
#' @details This function corresponds to \code{empty()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.empty.html}{see}) with the difference that instead of arbitrary values \code{NA} are set.
#' @return An array of \code{NA} with the given shape.
#'
#' @export
empty <- function(dim = NULL, dimnames = NULL) {
  marray(rep(NA, prod(dim)), dim = dim, dimnames = dimnames)
}

#' @rdname empty
#'
#' @param a An array whose shape is taken for the result array.
#'
#' @details This function corresponds to \code{empty_like()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.empty_like.html#numpy.empty_like}{see}).
#' @return An array of \code{NA} with the the same shape as \code{a}.
#'
#' @export
empty_like <- function(a) {
  empty(dim = DIM(a), dimnames = dimnames(a))
}

#' @title Array creation
#' @description Create array filled with value.
#'
#' @param dim Shape of the new array.
#' @param fill_value Value to fill the array.
#' @param dimnames Either \code{NULL} or the names of the dimensions. This must be a list with one component for each dimension, either \code{NULL} or a character vector of the length given by \code{dim} for that dimension.
#' @param order Whether to store multidimensional array in C- or Fortran-contiguous (row- or column-wise) order.
#'
#' @details This function corresponds to \code{full()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.full.html}{see}).
#' @return An array of \code{fill_value} with the given shape.
#'
#' @export
full <- function(dim = NULL, fill_value = NA, dimnames = NULL, order = c("C", "F")) {
  marray(rep(fill_value, prod(dim) -> N)[seq_len(N)], dim = dim, dimnames = dimnames, order = order)
}

#' @rdname full
#'
#' @param a An array whose shape is taken for the result array.
#'
#' @details This function corresponds to \code{full_like()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.full_like.html#numpy.full_like}{see}).
#' @return An array of \code{fill_value} with the the same shape as \code{a}.
#'
#' @export
full_like <- function(a, fill_value = NA, order = c("C", "F")) {
  full(dim = DIM(a), fill_value = fill_value, dimnames = dimnames(a), order = order)
}
