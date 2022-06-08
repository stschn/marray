#' @title Dimensions of an object
#' @description Set the dimensions of an object in row-major order.
#'
#' @param x An object to set dimensions on.
#' @param value An integerish vector of new dimensions.
#'
#' @return The (redimensioned) object \code{x}.
#'
#' @references Implementation credits go to \url{https://github.com/t-kalinowski/listarrays}.
#'
#' @export
`dimC<-` <- function(x, value) {
  if (is.null(value)) {
    if (is.null(dim(x)))
      return(x)

    if (ndim(x) > 1L)
      x <- transpose(x)

    dim(x) <- NULL
    return(x)
  }

  dx <- dim(x)
  if (identical(dx, as.integer(value)))
    return(x)

  if (!is.null(dx))
    x <- transpose(x)

  dim(x) <- rev(value)
  transpose(x)
}

#' @title Reshape an array
#'
#' @param a An array.
#' @param dim An integerish vector of new dimensions to be set on the array.
#' @param order The order in which elements of \code{a} should be read during rearrangement.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds to \code{reshape()} from NumPy.
#' @return The (redimensioned) array \code{a}.
#'
#' @seealso \code{\link[reticulate]{array_reshape}}.
#'
#' @export
reshape.array <- function(a, dim = NULL, order = c("C", "F")) {
  order <- match.arg(order)
  if (isTRUE(c("C") %in% order))
    dimC(a) <- dim
  else
    dim(a) <- dim
  a
}
