#' @title Array shape
#' @description Retrieve dimensions of an object or its length.
#'
#' @param x An R object.
#' @return The number of dimensions or in cases of an atomic object the length.
#' @references Implementation credits go to \url{https://github.com/t-kalinowski/listarrays}.
#' @export
DIM <- function(x) { dim(x) %||% length(x) }

#' @rdname DIM
#' @details The function \code{shape()} is a wrapper function for \code{DIM()}.
#' @export
shape <- function(x) { DIM(x) }

#' @title Array shape
#' @description Number of dimensions.
#'
#' @param x A multidimensional data structure like array, matrix or data.frame.
#' @details This function corresponds to \code{ndarray.ndim} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.ndarray.ndim.html}{see}).
#' @return Number of dimensions.
#' @export
ndim <- function(x) { length(dim(x)) }

#' @rdname ndim
#' @details The function \code{marank()} is a wrapper function for \code{ndim()}.
#' @export
marank <- function(x) { ndim(x) }

#' @title Array size
#' @description Number of elements.
#'
#' @param x A multidimensional data structure like array, matrix or data.frame.
#' @details This function corresponds to \code{ndarray.size} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.ndarray.size.html}{see}).
#' @return Number of elements.
#' @export
nsize <- function(x) { prod(dim(x)) }

#' @title Array shape
#' @description Ensure minimum number of dimensions.
#'
#' @param a An array.
#' @param n The desired number of dimensions.
#' @param axis Index position of the new axis in the expanded array. Negative numbers count from the back.
#'
#' @return The array \code{a} with at least \code{n} dimensions.
#' @export
ndmin <- function(a, n, axis = -1L) {
  while (ndim(a) < n) a <- expand_dims(a, axis)
  a
}

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
    if (is.null(dim(x) -> dx))
      return(x)

    if (length(dx) > 1L)
      x <- aperm(x)

    dim(x) <- NULL
    return(x)
  }

  dx <- dim(x)
  if (identical(dx, as.integer(value)))
    return(x)

  if (!is.null(dx))
    x <- aperm(x)

  dim(x) <- rev(value)
  aperm(x)
}

#' @title Array reshaping
#' @description Reshape an array.
#'
#' @param a An array.
#' @param dim An integerish vector of new dimensions to be set on the array. One dimension axis can be -1. In this case, the value is inferred from the size of the array and remaining dimensions.
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
  dim <- .standardize_shape(DIM(a), dim)
  if (identical(order, "C"))
    dimC(a) <- dim
  else
    dim(a) <- dim
  a
}

#' @title Array resizing
#' @description Resize an array.
#'
#' @param a An array.
#' @param dim An integerish vector of new shape (dimension space) to be set on the array.
#' @param type The technique to be used for creating the resized array.
#'   * \code{copy}: if the new array is larger, it's filled with repeated copies of \code{a}.
#'   * \code{zero}: if the new array is larger, it's filled with zeros.
#'   * \code{one}: if the new array is larger, it's filled with ones.
#'   * \code{na}: if the new array is larger, it's filled with NA.
#'   * \code{approx}: if the new array is larger, it's filled with approximated values.
#'   * \code{nearest_neighbor}: nearest-neighbor interpolation, also known as proximal interpolation, is a simple multivariate interpolation technique for one or more dimensions.
#'     If a dimension of the new array is a larger than the corresponding dimension of \code{a}, only every nth array element along this dimension is selected. Otherwise, an array element along this dimension is selected n times.
#' @md
#' @param order The order in which elements of \code{a} should be read during rearrangement.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds only partially to \code{resize()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.resize.html}{see}). The function from NumPy only uses the copy type.
#'
#' @return The new array \code{a} with given shape.
#'
#' @examples
#' a <- marray(c(10, 4, 22, 2, 18, 7, 9, 14, 25), dim = c(3, 3))
#' resize.array(a, dim = c(6, 6), type = "nearest_neighbor")
#'
#' @export
resize.array <- function(a, dim, type = c("copy", "zero", "one", "na", "approx", "nearest_neighbor"), order = c("C", "F")) {
  type <- match.arg(type)
  oldsize <- nsize(a)
  newsize <- prod(dim)
  switch(type,
    copy = {
      a <- flatten(a, order = order)
      if (newsize > oldsize) {
        repeats <- -floor(-newsize / oldsize)
        a <- rep(a, repeats)
      }
      return(marray(a[1L:newsize], dim = dim, order = order))
    },
    zero = {
      a <- flatten(a, order = order)
      if (newsize > oldsize) {
        repeats <- newsize - oldsize
        a <- c(a, rep(0, repeats))
      }
      return(marray(a[1L:newsize], dim = dim, order = order))
    },
    one = {
      a <- flatten(a, order = order)
      if (newsize > oldsize) {
        repeats <- newsize - oldsize
        a <- c(a, rep(1, repeats))
      }
      return(marray(a[1L:newsize], dim = dim, order = order))
    },
    na = {
      a <- flatten(a, order = order)
      if (newsize > oldsize) {
        repeats <- newsize - oldsize
        a <- c(a, rep(NA, repeats))
      }
      return(marray(a[1L:newsize], dim = dim, order = order))
    },
    approx = {
      a <- flatten(a, order = order)
      if (newsize > oldsize) {
        a <- stats::approx(a, n = newsize)$y
      }
      return(marray(a[1L:newsize], dim = dim, order = order))
    },
    nearest_neighbor = {
      d <- DIM(a)
      stopifnot("The new shape must have the same number of axes as the shape of a." = length(d) == length(dim))
      ratio <- dim / d
      ds <- lapply(dim, seq)
      idx <- lapply(seq_along(ds), function(i) {
        if (dim[i] < d[i])
          round(seq(1, d[i], by = d[i] / dim[i]))
        else
          ceiling(ds[[i]] / ratio[i])
      })
      return(slice(a, idx))
    }
  )
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

#' @title Expand the shape of an array
#' @description Insert new axis or axes that will appear at the axis positions in the expanded array shape.
#'
#' @param a An array.
#' @param axis Index position of the new axis or axes in the expanded array. Negative numbers count from the back.
#'
#' @details This function corresponds to \code{expand_dims()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.expand_dims.html}{see}).
#' @return The expanded array \code{a} with new shape.
#'
#' @seealso \code{\link{squeeze}}.
#'
#' @examples
#' a <- marray(1:24, dim = c(4, 3, 2))
#' DIM(expand_dims(a))
#' DIM(expand_dims(a, axis = c(1, -2)))
#' DIM(expand_dims(a, axis = c(-2, 3, -1)))
#' DIM(expand_dims(a, axis = c(1, 1, 3, -1)))
#'
#' @export
expand_dims <- function(a, axis = -1L) {
  a <- .standardize_array(a)
  d <- DIM(a)
  nd <- length(d)
  if (anyDuplicated(axis)) axis <- unique(axis)
  axis <- sort(axis)
  na <- length(axis)
  # Adopt negative axis positions
  neg <- axis < 0L
  if (any(neg))
    axis[neg] <- axis[neg] + nd + na + 1L
  # Adopt zero axis positions
  axis[axis == 0L] <- 1L
  # Create new dimension
  newdim <- vector(mode = "integer", length = nd + na)
  newdim[axis] <- 1L
  newdim[-axis] <- d
  #reshape.array(a, dim = newdim)
  dim(a) <- newdim
  a
}

#' @title Array compressing
#' @description Compress the shape of an array by removing singleton dimensions.
#'
#' @param a An array.
#' @param axis The axis or axes which should be removed. If \code{NULL} (default), all axes of length one are removed.
#' @param order The order in which elements of data should be read during rearrangement after removing of corresponding dimensions.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds to \code{squeeze()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.squeeze.html}{see}).
#'   The base R function \code{\link{drop}} does the same as this function. In opposite to \code{drop} this function
#'   allows reordering the elements of the newly created array as well as specifying only certain axes.
#'
#' @return The array \code{a} usually without axes of length one.
#'
#' @examples
#' a <- marray(seq(4*3*2), dim = c(1, 4, 3, 1, 2, 1))
#' DIM(squeeze(a))
#'
#' a <- marray(seq(4*3*2), dim = c(1, 4, 3, 1, 2, 1))
#' DIM(squeeze_first(a))
#'
#' a <- marray(seq(4*3*2), dim = c(1, 4, 3, 1, 2, 1))
#' DIM(squeeze_last(a))
#'
#' @seealso \code{\link{drop}}, \code{\link{expand_dims}}.
#'
#' @export
squeeze <- function(a, axis = NULL, order = c("C", "F")) {
  order <- match.arg(order)
  a <- .standardize_array(a)
  d <- DIM(a)
  if (is.null(axis)) {
    newdim <- d[!d %in% c(1L)]
  } else {
    axis <- .standardize_axis(axis, ndim(a))
    axis1 <- which(d %in% c(1L))
    remove_axis <- axis1[axis1 %in% axis]
    if (isFALSE((is.integer(remove_axis)) && (length(remove_axis) == 0L))) # check for integer (empty)
      newdim <- d[-remove_axis]
    else
      newdim <- d
  }
  marray(a, dim = newdim, order = order)
}

#' @rdname squeeze
#' @description Remove leading singleton dimensions.
#'
#' @export
squeeze_first <- function(a, order = c("C", "F")) {
  if (ndim(a) >= 2L)
    while ((DIM(a) -> d)[1L] == 1)
      a <- squeeze(a, axis = 1, order = order)
  a
}

#' @rdname squeeze
#' @description Remove trailing singleton dimensions.
#'
#' @export
squeeze_last <- function(a, order = c("C", "F")) {
  if (ndim(a) >= 2L)
    while ((DIM(a) -> d)[length(d)] == 1)
      a <- squeeze(a, axis = -1, order = order)
  a
}

#' @title Array enforcing and unravel
#' @description Enforce array or convert to vector.
#'
#' @param x A vector or array.
#' @return An array of at least one dimension.
#' @references Implementation credits go partially to \url{https://github.com/cran/arrayhelpers}.
#'
#' @examples
#' v <- setNames(1:11, do.call(paste0, list("i", c(1:11))))
#' v
#' class(v)
#' v <- ensuredim(v)
#' class(v)
#' dim(v)
#' dimnames(v)
#'
#' v <- dropdim(v)
#' class(v)
#' names(v)
#' dim(v)
#' dimnames(v)
#'
#' @export
ensuredim <- function(x) {
  if (is.null(dim(x)))
    x <- structure(x, .Dim = length(x),
                   .Dimnames = if (all(sapply(list(names(x)) -> xn, is.null))) NULL else xn,
                   .Names = NULL)
  x
}

#' @rdname ensuredim
#' @param order The order in which elements of \code{x} should be read during flattening if \code{x} is a higher-dimensional array.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#' @return A vector.
#' @export
dropdim <- function(x, order = c("C", "F")) {
  xn <- if (ndim(x) == 1L) dimnames(x)[[1L]] else names(x)
  x <- flatten(x, order = order)
  x <- structure(x, .Dim = NULL,
                 .Dimnames = NULL,
                 .Names = xn)
  x
}

#' @title Array enforcing
#' @description Convert inputs to arrays with at least one dimension.
#'
#' @param ... Any number of objects that are coerced into at least 1-D arrays.
#' @param order The order in which elements of the objects should be read during coercing to arrays.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#' @details This function corresponds to \code{atleast_1d()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.atleast_1d.html}{see}).
#' @return An array, or list of arrays, each with at least one dimension.
#' @export
atleast_1d <- function(..., order = c("C", "F")) {
  arys <- .dots(...)
  arys <- lapply(arys, FUN = marray, order = order)
  if (length(arys) == 1L) arys <- arys[[1L]]
  arys
}

#' @title Array enforcing
#' @description Convert inputs to arrays with at least two dimensions.
#'
#' @param ... Any number of objects that are coerced into at least 2-D arrays.
#' @param order The order in which elements of the objects should be read during coercing to arrays.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#' @details This function corresponds to \code{atleast_2d()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.atleast_2d.html}{see}).
#' @return An array, or list of arrays, each with at least two dimensions.
#' @export
atleast_2d <- function(..., order = c("C", "F")) {
  arys <- .dots(...)
  arys <- lapply(arys, FUN = marray, order = order)
  for (i in seq_along(arys)) {
    if (ndim(arys[[i]]) == 1L)
      arys[[i]] <- expand_dims(arys[[i]], axis = 1L)
  }
  if (length(arys) == 1L) arys <- arys[[1L]]
  arys
}

#' @title Array enforcing
#' @description Convert inputs to arrays with at least three dimensions.
#'
#' @param ... Any number of objects that are coerced into at least 3-D arrays.
#' @param order The order in which elements of the objects should be read during coercing to arrays.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#' @details This function corresponds to \code{atleast_3d()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.atleast_3d.html}{see}).
#' @return An array, or list of arrays, each with at least three dimensions.
#' @export
atleast_3d <- function(..., order = c("C", "F")) {
  arys <- .dots(...)
  arys <- lapply(arys, FUN = marray, order = order)
  for (i in seq_along(arys)) {
    nd <- ndim(arys[[i]])
    if (nd == 1L)
      arys[[i]] <- expand_dims(arys[[i]], axis = c(1, -1))
    else if (nd == 2L)
      arys[[i]] <- expand_dims(arys[[i]])
  }
  if (length(arys) == 1L) arys <- arys[[1L]]
  arys
}
