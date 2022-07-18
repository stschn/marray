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

#' @title Array reshaping
#' @description Reshape an array.
#'
#' @param a An array.
#' @param dim An integerish vector of new dimensions to be set on the array.
#' @param order The order in which elements of \code{a} should be read during rearrangement.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds to \code{reshape()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.reshape.html}{see}).\cr
#' Reshaping an array causes the dimension space (shape) to change. This can result in new dimensions (axes) as well as changes in dimension lengths.
#' In contrast to transpositions, which also lead to changes in the dimension space, the layout of the underlying data, i.e. their order, remains unchanged.
#' The data are flattened and then arranged into the new dimension space while retaining their order.
#'
#' @return The (redimensioned) array \code{a}.
#'
#' @seealso \code{\link[reticulate]{array_reshape}}.
#'
#' @export
reshape.array <- function(a, dim = NULL, order = c("C", "F")) {
  order <- match.arg(order)
  if (identical(order, "C"))
    dimC(a) <- dim
  else
    dim(a) <- dim
  a
}

#' @title Expand the shape of an array
#' @description Insert a new axis that will appear at the axis position in the expanded array shape.
#'
#' @param a An array.
#' @param axis Index position of the new axis in the expanded array. Negative numbers count from the back.
#'
#' @details This function corresponds to \code{expand_dims()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.expand_dims.html}{see}).
#' @return The expanded array \code{a} with new shape.
#'
#' @references Implementation credits go to \url{https://github.com/t-kalinowski/listarrays}.
#'
#' @seealso \code{\link{squeeze}}.
#'
#' @export
expand_dims <- function(a, axis = -1L) {
  d <- DIM(a)
  nd <- length(d)
  naxis <- length(axis)

  wd <- axis
  neg <- wd < 0L
  if (any(neg))
    wd[neg] <- wd[neg] + nd + naxis + 1L

  if (min(wd) < 1L)
    stop("implicit additional dims for expansions with negative indexes are not supported.")

  if ((max_wd <- max(wd)) > nd + naxis) {
    # Implicitly pad on right
    wd <- unique(c(wd), (nd + 1L):max_wd)
    ndout <- max_wd
  } else
    ndout <- nd + naxis

  if (anyDuplicated(wd)) {
    wd <- unique(wd)
  }

  dims <- rep(1L, ndout)
  dims[-wd] <- d

  dim(a) <- dims
  a
}

#' @title Array compressing
#' @description Compress the shape of an array by removing singleton dimensions.
#'
#' @param a An array.
#' @param axis The dimensions which should be removed. If \code{NULL} (default), all dimensions of length one are removed.
#' @param order The order in which elements of data should be read during rearrangement after removing of corresponding dimensions.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds to \code{squeeze()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.squeeze.html}{see}).
#'   The base R function \code{\link{drop}} does the same as this function. In opposite to \code{drop} this function
#'   allows reordering the elements of the newly created array as well as specifying only certain axes.
#'
#' @return The array \code{a} usually without dimensions of length one.
#'
#' @seealso \code{\link{drop}}, \code{\link{expand_dims}}.
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

#' @title Array resizing
#' @description Resize an array.
#'
#' @param a An array.
#' @param dim An integerish vector of new shape (dimension space) to be set on the array.
#' @param fill The type of fill method. If the new array is larger than the original array, then the new array is filled with:
#'   * \code{copy}: repeated copies of \code{a}
#'   * \code{zero}: zeros
#'   * \code{one}: ones
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
resize.array <- function(a, dim, fill = c("copy", "zero", "one", "na", "approx"), order = c("C", "F")) {
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
      one = {
        repeats <- newsize - oldsize
        a <- c(a, rep(1, repeats))
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

#' @title Retrieve broadcast dimensions
#'
#' @param ... Any number of objects.
#' @param axis The axis along operations like binding are planned.
#'
#' @return The dimension all objects must meet in order for the operation to be performed.
#'
#' @seealso \code{\link{reshape_broadcast}}.
#'
#' @export
broadcastDIM <- function(..., axis = NULL)  {
  arys <- lapply(.dots(...), FUN = marray)
  N <- max(1L, sapply(arys, ndim))
  arys <- lapply(arys, ndmin, n = N)
  dims <- sapply(arys, dim)
  if (is.vector(dims)) dims <- t(as.matrix(dims))
  dims <- apply(dims, 1L, max)
  if (!is.null(axis)) {
    nd <- length(dims)
    axis[which((axis <= 0) | (axis > nd))] <- nd
    dims[axis] <- NA
  }
  dims
}

#' @title Reshape arrays on basis of broadcasting
#'
#' @param ... Any number of objects.
#' @param axis The axis along operations like binding are planned.
#'
#' @return An array, or list of arrays, each with a dimension necessary for planned operations along axis.
#'
#' @seealso \code{\link{broadcastDIM}}.
#'
#' @export
reshape_broadcast <- function(..., axis = NULL) {
  arys <- .dots(...)
  if (length(arys) == 1L) return(arys[[1L]])
  bdim <- broadcastDIM(..., axis = axis)
  arys <- lapply(arys, FUN = marray)
  arys <- lapply(arys, function(a) {
    if (!all(is.na(setdiff(bdim, DIM(a))))) {
      bdim[is.na(bdim)] <- 1L
      a <- reshape.array(a, dim = bdim)
    }
    a
  })
  arys
}
