#' @title Array compressing
#' @description Compress the shape of an array by removing singleton dimensions.
#'
#' @param a An array.
#' @param axis The dimensions which should be removed. If \code{NULL} (default), all dimensions of length one are removed.
#' @param order The order in which elements of data should be read during rearrangement after removing of corresponding dimensions.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds to \code{squeeze()} from NumPy.
#'   The base R function \code{\link{drop}} does the same as this function. In opposite to \code{drop} this function
#'   allows reordering the elements of the newly created array as well as specifying only certain axes.
#'
#' @return The array \code{a} usually without dimensions of length one.
#'
#' @seealso \code{\link{drop}}.
#'
#' @export
squeeze <- function(a, axis = NULL, order = c("C", "F")) {
  order <- match.arg(order)
  d <- dim(a)
  if (is.null(axis)) {
    newdim <- d[!d %in% c(1L)]
  } else {
    axis1 <- which(d %in% c(1L))
    remove_axis <- axis1[axis1 %in% axis]
    if (isFALSE((is.integer(remove_axis)) && (length(remove_axis) == 0L))) # check for integer (empty)
      newdim <- d[-remove_axis]
    else
      newdim <- d
  }
  marray(a, dim = newdim, order = order)
}
