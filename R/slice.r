#' @title Array slicing
#' @description Slice an array by using indices i, j, k etc.
#'
#' @param a A vector, matrix, or array.
#' @param ... Indexing instructions in form of \code{name = value} pairs. The names of the arguments specify the dimensions and the values its values.
#' @param value Any values to assign to the slice of \code{a}.
#' @param drop For matrices and arrays. If \code{TRUE} the result is coerced to the lowest possible dimension. This only works for extracting elements, not for the replacement. See \code{\link[base]{drop}} for further details.
#'
#' @details \code{slice} is an alternative way to handle indexing array objects, usually done with \code{\link[base]{[}}. The dimensions must be indexed by names,
#'   i for the first dimension, j for the second and so on. The assigned values are the values (elements) of the corresponding dimension. The indexing expressions are the same as for \code{\link[base]{[}}.
#'
#' @return An extracted part of \code{a}.
#'
#' @references Implementation credits go to \url{https://github.com/cran/arrayhelpers}.
#'
#' @examples
#' a <- marray(1:48, dim = c(4, 3, 2, 2))
#' slice(a) # complete four-dimensional array
#' slice(a, l = 2) # the values of the array of the second element of the last dimension (4th dimension)
#' slice(a, i = 1, j = 3) # the values of the array of the first element of the first dimension (1st row) and the third element of the second dimension (3rd column) across all bunches of the remaining dimensions 3 and 4.
#'
#' a <- marray(1:24, dim = c(4, 3, 2), order = "F")
#' slice(a, i = 1L) <- 0L # write 0 to the first dimension (row) across all remaining dimensions
#' slice(a, i = 1L) <- 100:102 # write 100-102 to the first dimension (row) across all remaining dimensions
#' slice(a, i = 1L) <- 100:105 # write 100-105 to the first dimension (row) across all remaining dimensions
#' slice(a, i = 1L) <- matrix(100:105, nrow = 2L) # equal to prior, nrow can be 1, 2, 3, or 6
#' @export
slice <- function(a, ..., drop = TRUE) {
  args <- as.list(rep(TRUE, ndim(a)))
  params <- list(...)
  which <- match(names(params), letters) - 8L
  args[which] <- params
  do.call(`[`, c(list(a), args, list(drop = drop)))
}

#' @rdname slice
#' @usage \code{slice(a, ...) <- value}.
#' @export
'slice<-' <- function(a, ..., value) {
  args <- as.list(rep(TRUE, ndim(a)))
  params <- list(...)
  which <- match(names(params), letters) - 8L
  args[which] <- params
  do.call(`[<-`, c(list(a), args, list(value = value)))
}
