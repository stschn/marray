#' @rdname random_array
#' @title Array creation
#' @description Create array filled with random integer or double values.
#'
#' @param dim Shape of the new array.
#' @param min,max Lower and upper limits of the generated random values.
#' @param dimnames Either \code{NULL} or the names of the dimensions. This must be a list with one component for each dimension, either \code{NULL} or a character vector of the length given by \code{dim} for that dimension.
#' @param order Whether to store multidimensional array in C- or Fortran-contiguous (row- or column-wise) order.
#'
#' @details This function corresponds roughly to \code{random.rand()} from NumPy (\href{https://numpy.org/doc/stable/reference/random/generated/numpy.random.rand.html}{see}).
#' @return An array with random values with the given shape.
#'
#' @export
random_int <- function(dim = NULL, min = 0, max = 1, dimnames = NULL, order = c("C", "F")) {
  marray(sample(x = c(min:max), size = prod(dim), replace = TRUE), dim = dim, dimnames = dimnames, order = order)
}

#' @rdname random_array
#' @param a An array whose shape is taken for the result array.
#' @export
random_int_like <- function(a, min = 0, max = 1, dimnames = NULL, order = c("C", "F")) {
  random_int(dim = DIM(a), min = min, max = max, dimnames = dimnames, order = order)
}

#' @rdname random_array
#' @export
random_dbl <- function(dim = NULL, min = 0, max = 1, dimnames = NULL, order = c("C", "F")) {
  marray(runif(n = prod(dim), min = min, max = max), dim = dim, dimnames = dimnames, order = order)
}

#' @rdname random_array
#' @export
random_dbl_like <- function(a, min = 0, max = 1, dimnames = NULL, order = c("C", "F")) {
  random_dbl(dim = DIM(a), min = min, max = max, dimnames = dimnames, order = order)
}

#' @rdname random_array
#' @details This function corresponds to \code{random.normal()} from NumPy (\href{https://numpy.org/doc/stable/reference/random/generated/numpy.random.normal.html}{see}).
#' @export
random_normal <- function(dim = NULL,  mean = 0, sd = 1, order = c("C", "F")) {
  marray(rnorm(n = prod(dim), mean = mean, sd = sd), dim = dim, order = order)
}
