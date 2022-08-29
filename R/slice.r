#' @title Array slicing
#' @description Read and write slices of an array using enhanced indexing operations.
#'
#' @param a A vector, matrix, or array.
#' @param ... Indexing instructions with or without letters in form of \code{name = value} pairs. The names of the arguments specify the axis and the values its positions.
#' @param value Any values to assign to the slice of \code{a}.
#' @param drop For matrices and arrays. If \code{TRUE} the result is coerced to the lowest possible dimension. This only works for extracting elements, not for the replacement. See \code{\link[base]{drop}} for further details.
#'
#' @details \code{slice} is an alternative way to handle indexing array objects, usually done with \code{\link[base]{[}}. The dimensions can be indexed by names,
#'   i for the first dimension, j for the second and so on. The assigned values are the positions on the corresponding dimension. The indexing expressions are the same as for \code{\link[base]{[}}.
#'
#' @return An extracted part of \code{a}.
#'
#' @references Indexing by letters is inspired by \url{https://github.com/cran/arrayhelpers}.
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
slice <- function(a, ..., drop = FALSE) {
  args <- .aindex(DIM(a), ...)
  do.call(`[`, c(list(a), args, list(drop = drop)))
}

#' @rdname slice
#' @usage \code{slice(a, ...) <- value}.
#' @export
'slice<-' <- function(a, ..., value) {
  args <- .aindex(DIM(a), ...)
  do.call(`[<-`, c(list(a), args, list(value = value)))
}

#' @title Array Indexes
#' @description Extract indices of axes of an array.
#'
#' @param a An array.
#' @param value Any number of values to search for in \code{a}.
#'
#' @details If there is no match between \code{value} and the values of \code{a}, \code{NA} will be returned.
#'
#' @return A list of matrices with the indices of the dimension space of \code{value} within \code{a}, otherwise \code{NA}.
#'
#' @examples
#' a <- marray(1:24, dim = c(4, 3, 2))
#' axesindices(a, value = c(11, 2, 23))
#'
#' a <- marray(sample(6, 24, replace = TRUE), dim = c(4, 3, 2))
#' axesindices(a, value = c(4, 6, 0))
#'
#' a <- ones(dim = c(4, 3, 2))
#' axesindices(a, value = 1)
#'
#' @export
axesindices <- function(a, value) {
  a <- .standardize_array(a)
  # a loop is necessary because which(a %in% value) ignores the order within value
  lapply(value, function(v) {
    i <- which(a %in% v) %|0|% NA
    if (!any(is.na(i)))
      arrayInd(i, DIM(a)) # arrayInd(seq_along(a), DIM(a)) returns all axes indices
    else
      i
  })
}

#' @title Array searching
#' @description Return elements of an array that satisfy conditions.
#'
#' @param a An array.
#' @param condition Logical expression indicating elements to keep.
#'
#' @details This function is an equivalent to \code{extract()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.extract.html}{see}).
#'
#' @return An array with rank 1 containing elements of \code{a} that meet \code{condition}, otherwise invisible \code{NULL}.
#'
#' @examples
#' a <- marray(1:24, dim = c(4, 3, 2))
#' values <- extract(a, condition = (a > 3) & (a <= 11))
#' values
#'
#' # Forward the values to axesindices() to get the coordinates of these values
#' axesindices(a, value = values)
#'
#' @export
extract <- function(a, condition) {
  a <- .standardize_array(a)
  if (missing(condition))
    return(a)
  e <- substitute(condition)
  idx <- eval(e, parent.frame())
  if (!is.logical(idx))
    stop("'condition' must be logical.", call. = FALSE)
  idx <- idx & !is.na(idx)
  out <- a[idx]
  if (length(out)) as.array(out) else invisible(NULL)
}
