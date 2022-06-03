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
#' @details This function corresponds to \code{ndarray.ndim} from NumPy.
#' @return Number of dimensions.
#' @export
ndim <- function(x) { length(dim(x)) }

#' @title Number of elements
#'
#' @param x A multidimensional data structure like array, matrix or data.frame.
#' @details This function corresponds to \code{ndarray.size} from NumPy.
#' @return Number of elements.
#' @export
nsize <- function(x) { prod(dim(x)) }
