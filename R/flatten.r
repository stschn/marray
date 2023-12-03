#' @title Data flattening
#' @description Flatten data into a one-dimensional array or into a vector of corresponding type.
#'
#' @param data Data to be flatten.
#' @param axis The axes to be fixed while iterating over the remaining axes of \code{data}.
#'   By default (\code{NULL}), the structure of \code{data} is interpret as a stack (of stack...) of matrices,
#'   with either the first axis (\code{C}-order) or the second axis (\code{F}-order) and all remaining axes are fixed.
#' @param order The order in which elements of \code{data} should be read during flattening.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds to \code{ndarray.flatten()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.ndarray.flatten.html}{see}).
#' @return The flatten data in form of a one-dimensional array.
#'   \code{flatten_int()} returns an integer vector, \code{flatten_dbl()} a double vector, \code{flatten_raw()} a raw vector, \code{flatten_cpx()} a complex vector \code{flatten_chr()} a character vector, and \code{flatten_lgl()} a logical vector.
#'
#' @examples
#' v <- (1:24); dim(v); ndim(v)
#' l <- list(x1 = 1:10, x2 = seq(10, 100, 10), x3 = list(a = 11, b = c(2, 23)))
#' m <- matrix(1:24, nrow = 6); dim(m); length(m);
#' a3 <- array(v, dim = c(4, 3, 2)); dim(a3); length(a3)
#' a4 <- array(1:48, dim = c(4, 3, 2, 2))
#' data <- a4; data
#' flatten(data, order = "F"); flatten(data, order = "C")
#' @export
flatten <- function(data, axis = NULL, order = c("C", "F")) {
  order <- match.arg(order)
  if ((!all(is.na(data))) && (is.atomic(data)) && (!(ndim(data) > 1L))) {
    data <- array(data)
  } else {
  if (is.data.frame(data)) {
    data <- as.matrix(data)
  } else {
  if (is.list(data)) {
    data <- array(unlist(data))
  }}}
  if (!is.null(axis))
    data <- apply(data, MARGIN = axis, FUN = identity)
  return(as.array(as.vector(reshape.array(data, order = order))))
}

#' @rdname flatten
#' @details The function \code{ravel()} is a wrapper function for \code{flatten()}.
#'   It corresponds to \code{ndarray.ravel()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.ravel.html}{see}).
#' @export
ravel <- function(data, order = c("C", "F")) {
  flatten(data, order = order)
}

#' @rdname flatten
#' @export
flatten_int <- function(data, order = c("C", "F")) {
  as.integer(dropdim(data, order = order))
}

#' @rdname flatten
#' @export
flatten_dbl <- function(data, order = c("C", "F")) {
  as.double(dropdim(data, order = order))
}

#' @rdname flatten
#' @export
flatten_raw <- function(data, order = c("C", "F")) {
  as.raw(dropdim(data, order = order))
}

#' @rdname flatten
#' @export
flatten_cpx <- function(data, order = c("C", "F")) {
  as.complex(dropdim(data, order = order))
}

#' @rdname flatten
#' @export
flatten_chr <- function(data, order = c("C", "F")) {
  as.character(dropdim(data, order = order))
}

#' @rdname flatten
#' @export
flatten_lgl <- function(data, order = c("C", "F")) {
  as.logical(dropdim(data, order = order))
}

#' @title Array indexing
#' @description Converts a list of indices into an array of flat indices.
#'
#' @param multi_index A list of indexes, one element for each dimension.
#' @param dims The shape of array into which the indices from \code{multi_index} apply.
#' @param order The order in which elements of the array should be read.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds to \code{ndarray.ravel_multi_index()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.ravel_multi_index.html}{see}).
#' @return An array of indices into the flattened version of an array of dimensions \code{dims}.
#'
#' @examples
#' # Same examples as for NumPy
#' ravel_multi_index(multi_index = list(c(4, 7, 7), c(5, 6, 2)), dims = c(7, 6)) - 1 # indexing in Python starts at 0
#' ravel_multi_index(multi_index = list(c(4, 7, 7), c(5, 6, 2)), dims = c(7, 6), order = "F") - 1
#' ravel_multi_index(list(4, 2, 5, 2), dims = c(6, 7, 8, 9)) - 1
#'
#' @export
ravel_multi_index <- function(multi_index, dims, order = c("C", "F")) {
  a <- marray(seq(prod(dims)), dim = dims, order = order)
  mi_len <- unique(unlist(lapply(multi_index, length)))
  stopifnot("Length of 'multi_index' must be equal to the length of 'dims'." = length(multi_index) == length(dims),
            "All elements within 'multi_index' must have the same length." = length(mi_len) == 1L)
  return(unlist(lapply(seq_len(mi_len), function(i) { slice(a, sapply(multi_index, `[`, i, simplify = FALSE)) })))
}
