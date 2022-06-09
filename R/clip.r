#' @title Array clip
#' @description Clip (limit) the values in an array.
#'
#' @param a An array.
#' @param ... Indexing instructions with or without letters in form of \code{name = value} pairs. The names of the arguments specify the dimensions and the values its values.
#' @param a_min Minimum value.
#' @param a_max Maximum value.
#'
#' @details This function corresponds partially to \code{clip()} from NumPy.
#' In opposite to NumPy function a slice of \code{a} can be specified to which limitation of the values refers.
#'
#' @return The array \code{a}, but where values < \code{a_min} are replaced with \code{a_min}, and those > \code{a_max} with \code{a_max}.
#'
#' @examples
#' a <- marray(1:24, dim = c(4, 3, 2))
#' # Limit entire array
#' maclip(a, a_min = 3, a_max = 7)
#' # Limit only a region of the array
#' maclip(a, i = 1:2, j = 2:3, a_min = 3, a_max = 7)
#'
#' @export
maclip <- function(a, ..., a_min, a_max) {
  as <- slice(a, ...)
  if (a_min > a_max)
    slice(as) <- a_max
  else {
    as[as < a_min] <- a_min
    as[as > a_max] <- a_max
  }
  slice(a, ...) <- as
  a
}
