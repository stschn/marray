#' NULL assignment operator
#'
#' @param x object that is checked for \code{NULL}.
#' @param y result if \code{x} is \code{NULL}.
#'
#' @details Checking for \code{NULL} is very common practice in R. This operator shortens the handling of such checks and the return of corresponding values.
#'   It is adopted from package \code{rlang}, also used in package \code{listarrays}.
#'
#' @return Either \code{x} or \code{y}.
#' @name op-null-default
#' @examples
#' 11 %||% 23
#' NULL %||% 23
#' @export
`%||%` <- function(x, y) { if (is.null(x)) y else x }

#' Zero length assignment operator
#'
#' @param x object that is checked for \code{length}.
#' @param y result if length of \code{x} is \code{0}.
#'
#' @return Either \code{x} or \code{y}.
#' @name op-zero-length-default
#' @export
`%|0|%` <- function(x, y) { if (!length(x)) y else x }

#' Value matching with dimension preserving
#'
#' @param x an array.
#' @param table the values to be matched against.
#'
#' @details The base R operator \code{\%in\%} doesn't preserve the dimension of the object values are matched against.
#'
#' @return A logical array with the same shape as \code{x}, indicating if a match was located for each element of \code{x}; thus the values are \code{TRUE} or \code{FALSE}.
#' @name op-in-dim
#' @examples
#' a <- marray(sample(4, 12, replace = TRUE), dim = c(4, 3, 1))
#' a %[in]% c(3)
#' @export
`%[in]%` <- function(x, table) {
  stopifnot(is.array(x))
  r <- x %in% table
  dim(r) <- dim(x)
  r
}
