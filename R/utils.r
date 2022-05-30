#' NULL assignment operator
#'
#' @param x object that is checked for \code{NULL}.
#' @param y result if \code{x} is \code{NULL}.
#'
#' @details Checking for \code{NULL} is very common practice in R. This operator shortens the handling of such checks and the return of corresponding values.
#'
#' @return Either \code{x} or \code{y}.
#' @export
`%null%` <- function(x, y) { if (is.null(x)) y else x }

#' @title Retrieve dimensions of an object or its length
#'
#' @param x An R object.
#' @return The number of dimensions or in cases of an atomic object the length.
#' @references Implementation credits go to \url{https://github.com/t-kalinowski/listarrays}.
#' @export
DIM <- function(x) { dim(x) %null% length(x) }

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
