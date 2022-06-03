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
'%||%' <- function(x, y) { if (is.null(x)) y else x }
'%|0|%' <- function(x, y) { if (!length(x)) y else x }
