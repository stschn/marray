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
#' @details This function corresponds to \code{reshape()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.reshape.html}{see}).
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

#' @title Resize an array
#'
#' @param a An array.
#' @param dim An integerish vector of new shape (dimension space) to be set on the array.
#' @param fill The type of fill method. If the new array is larger than the original array, then the new array is filled with:
#'   * \code{copy}: repeated copies of \code{a}
#'   * \code{zero}: zeros
#'   * \code{na}: NA
#'   * \code{approx}: approximated values
#' @md
#' @param order The order in which elements of \code{a} should be read during rearrangement.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds partially to \code{resize()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.resize.html}{see}). The function from NumPy only uses the copy type.
#'
#' @return The new array \code{a} with given shape.
#'
#' @export
resize.array <- function(a, dim, fill = c("copy", "zero", "na", "approx"), order = c("C", "F")) {
  fill <- match.arg(fill)
  oldsize <- nsize(a)
  newsize <- prod(dim)
  a <- flatten(a, order = order)

  if (newsize > oldsize)
    switch(fill,
      copy = {
        repeats <- -floor(-newsize / oldsize)
        a <- rep(a, repeats)
      },
      zero = {
        repeats <- newsize - oldsize
        a <- c(a, rep(0, repeats))
      },
      na = {
        repeats <- newsize - oldsize
        a <- c(a, rep(NA, repeats))
      },
      approx = {
        a <- stats::approx(a, n = newsize)$y
      },
    )

  marray(a[1L:newsize], dim = dim, order = order)
}
