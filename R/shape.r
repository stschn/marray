#' @title Retrieve dimensions of an object or its length
#'
#' @param x An R object.
#' @return The number of dimensions or in cases of an atomic object the length.
#' @references Implementation credits go to \url{https://github.com/t-kalinowski/listarrays}.
#' @export
DIM <- function(x) { dim(x) %||% length(x) }

#' @title Number of dimensions
#'
#' @param x A multidimensional data structure like array, matrix or data.frame.
#' @details This function corresponds to \code{ndarray.ndim} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.ndarray.ndim.html}{see}).
#' @return Number of dimensions.
#' @export
ndim <- function(x) { length(dim(x)) }

#' @title Number of elements
#'
#' @param x A multidimensional data structure like array, matrix or data.frame.
#' @details This function corresponds to \code{ndarray.size} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.ndarray.size.html}{see}).
#' @return Number of elements.
#' @export
nsize <- function(x) { prod(dim(x)) }

#' @title Enforce array and convert to vector
#'
#' @param x A vector or array.
#' @return An array of at least one dimension.
#' @references Implementation credits go partially to \url{https://github.com/cran/arrayhelpers}.
#'
#' @examples
#' v <- setNames(1:11, do.call(paste0, list("i", c(1:11))))
#' v
#' class(v)
#' v <- ensuredim(v)
#' class(v)
#' dim(v)
#' dimnames(v)
#'
#' v <- dropdim(v)
#' class(v)
#' names(v)
#' dim(v)
#' dimnames(v)
#'
#' @export
ensuredim <- function(x) {
  if (is.null(dim(x)))
    x <- structure(x, .Dim = length(x),
                   .Dimnames = if (all(sapply(list(names(x)) -> xn, is.null))) NULL else xn,
                   .Names = NULL)
  x
}

#' @rdname ensuredim
#' @param order The order in which elements of \code{x} should be read during flattening if \code{x} is a higher-dimensional array.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#' @return A vector.
#' @export
dropdim <- function(x, order = c("C", "F")) {
  xn <- if (ndim(x) == 1L) dimnames(x)[[1L]] else names(x)
  x <- flatten(x, order = order)
  x <- structure(x, .Dim = NULL,
                 .Dimnames = NULL,
                 .Names = xn)
  x
}

#' @title Convert inputs to arrays with at least one dimension
#'
#' @param ... Any number of objects that are coerced into at least 1-D arrays.
#' @param order The order in which elements of the objects should be read during coercing to arrays.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#' @details This function corresponds to \code{atleast_1d()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.atleast_1d.html}{see}).
#' @return An array, or list of arrays, each with at least one dimension.
#' @export
atleast_1d <- function(..., order = c("C", "F")) {
  arys <- .dots(...)
  arys <- lapply(arys, FUN = marray, order = order)
  if (length(arys) == 1L) arys <- arys[[1L]]
  arys
}

#' @title Convert inputs to arrays with at least two dimensions
#'
#' @param ... Any number of objects that are coerced into at least 2-D arrays.
#' @param order The order in which elements of the objects should be read during coercing to arrays.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#' @details This function corresponds to \code{atleast_2d()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.atleast_2d.html}{see}).
#' @return An array, or list of arrays, each with at least two dimensions.
#' @export
atleast_2d <- function(..., order = c("C", "F")) {
  arys <- .dots(...)
  arys <- lapply(arys, FUN = marray, order = order)
  for (i in seq_along(arys)) {
    if (ndim(arys[[i]]) == 1L)
      arys[[i]] <- expand_dims(arys[[i]], axis = 1L)
  }
  if (length(arys) == 1L) arys <- arys[[1L]]
  arys
}

#' @title Convert inputs to arrays with at least three dimensions
#'
#' @param ... Any number of objects that are coerced into at least 3-D arrays.
#' @param order The order in which elements of the objects should be read during coercing to arrays.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#' @details This function corresponds to \code{atleast_3d()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.atleast_3d.html}{see}).
#' @return An array, or list of arrays, each with at least three dimensions.
#' @export
atleast_3d <- function(..., order = c("C", "F")) {
  arys <- .dots(...)
  arys <- lapply(arys, FUN = marray, order = order)
  for (i in seq_along(arys)) {
    if (ndim(arys[[i]]) == 1L)
      arys[[i]] <- reshape.array(arys[[i]], dim = c(1L, DIM(arys[[i]]), 1L))
    else if (ndim(arys[[i]] == 2L))
      arys[[i]] <- expand_dims(arys[[i]])
  }
  if (length(arys) == 1L) arys <- arys[[1L]]
  arys
}
