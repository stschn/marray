#' @title Multidimensional array
#' @description
#'   \code{marray(data, ...)} creates a reshaped multidimensional array.\cr
#'   \code{as.marray(data, ...)} attempts to turn its argument into an array.\cr
#'
#' @param data The data to be reshaped to a multidimensional array.
#' @param dim The dimensions for the created array. If \code{dim} is not defined (default) and \code{data} already has dimensions, these will be applied.
#' @param dimnames Either \code{NULL} or the names of the dimensions. This must be a list with one component for each dimension, either \code{NULL} or a character vector of the length given by \code{dim} for that dimension.
#' @param order The order in which elements of data should be read during rearrangement.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#' @param encode The mode to be used for converting a factor object into an array. The mode \code{onehot} builds a matrix with as many columns as the factor variable has levels, where each column corresponds to a level.
#'   The mode \code{sparse} creates a matrix with only one column, whose values correspond to the ordinal value of a level. The mode \code{NULL} stands for no encoding.
#'
#' @details This introduced n-dimensional array is an equivalent to \code{ndarray} class from NumPy (\url{https://numpy.org/}), a famous package in Python.
#'   Usually, an n-dimensional array is a multidimensional container consisting of bunches of bunches of bunches... of matrices.
#'   The first two dimensions define the matrix while the remaining dimensions define the corresponding bunches. For e.g., an 4x3x2 array has 2 bunches of each 4x3 matrix.
#'   An 6x4x3x2 array has 2 bunches, each of these two bunches has 3 bunches and each of these three bunches again contains a 6x4 matrix.
#'
#'   The behavior of \code{marray} is similar to that of ndarray from NumPy. R follows a column-major ordering (Fortran-style) during building up an array,
#'   wile Python respectively NumPy prefers row-major ordering (C-style) but offers both. For a comparison see \url{https://rstudio.github.io/reticulate/articles/arrays.html}.
#'
#' @return An array.
#'
#' @seealso \code{\link{array}}, \code{\link{dim}}, \code{\link{reshape.array}}.
#'
#' @examples
#' # Vector input with explicit dimensions
#' marray(1:24, dim = c(8, 3)) # 2D array with row-major ordering
#' marray(1:24, dim = c(8, 3), order = "F") # 2D array with column-major ordering
#' marray(1:24, dim = c(4, 3, 2)) # 3D array with row-major ordering
#' marray(1:24, dim = c(4, 3, 2), order = "F") # 3D array with column-major ordering
#'
#' # Different input types and applying the dimensions
#' v <- (1:24)
#' l <- list(x1 = 1:10, x2 = seq(10, 100, 10))
#' df <- data.frame(x1 = 1:6, x2 = seq(10, 60, 10), x3 = sample(letters, 6))
#' m <- matrix(1:24, nrow = 6)
#' a1 <- array(letters[1L:24L])
#' a3 <- array(v, dim = c(4, 3, 2))
#' a4 <- array(1:48, dim = c(4, 3, 2, 2))
#' data <- a3; data
#' a <- marray(data, order = "F"); a
#'
#' # Convert array elements into specific type
#' a <- marray(sample(c(0, 1), 24, replace = TRUE), dim = c(4, 3 ,2, 1)) |>
#'   as.marray_lgl()
#' a
#'
#' # Convert factor variable into an array
#' x <- factor(sample(c("cloudy", "sunny", "foggy", "rainy", "snowy") -> lvls, size = 20, replace = TRUE), levels = lvls)
#' marray(x, encode = "onehot")
#' marray(x, encode = "sparse")
#'
#' @export
marray <- function(data, ...) {
  as.marray(data, ...)
}

#' @rdname marray
#' @export
as.marray <- function(data, ...) {
  UseMethod("as.marray")
}

#' @rdname marray
#' @export
as.marray.default <- function(data, dim = NULL, dimnames = NULL, order = c("C", "F")) {
  order <- match.arg(order)
  if (is.null(dim)) dim <- DIM(data)
  if (!is.array(data)) data <- array(data)
  data <- reshape.array(a = data, dim = dim, order = order)

  if (!is.null(dimnames)) { dimnames(data) <- dimnames }
  data
}

#' @rdname marray
#' @export
as.marray.matrix <- function(data, dim = NULL, dimnames = NULL, order = c("C", "F")) {
  if (is.null(dimnames)) dimnames <- dimnames(data)
  as.marray.default(data, dim = dim, dimnames = dimnames, order = order)
}

#' @rdname marray
#' @export
as.marray.table <- function(data, dim = NULL, dimnames = NULL, order = c("C", "F")) {
  if (is.null(dimnames)) dimnames <- dimnames(data)
  # table is an array with extra class "table"
  as.marray.default(unclass(data), dim = dim, dimnames = dimnames, order = order)
}

#' @rdname marray
#' @export
as.marray.data.frame <- function(data, dim = NULL, dimnames = NULL, order = c("C", "F")) {
  if (is.null(dimnames)) dimnames <- dimnames(data)
  as.marray.default(as.matrix(data), dim = dim, dimnames = dimnames, order = order)
}

#' @rdname marray
#' @export
as.marray.list <- function(data, dim = NULL, dimnames = NULL, order = c("C", "F")) {
  as.marray.default(array(unlist(data)), dim = dim, dimnames = dimnames, order = order)
}

#' @rdname marray
#' @export
as.marray.factor <- function(data, encode = c("onehot", "sparse")) {
  if (is.null(encode)) return(as.marray.default(data))
  encode <- match.arg(encode)
  switch(encode,
    onehot = {
      a <- eye(length(levels(data) -> lvls), dimnames = list(NULL, lvls))
      a <- a[as.integer(data), ]
      return(a)
    },
    sparse = {
      a <- expand_dims(as.integer(data) - 1L)
      e <- substitute(data)
      e <- do.call(substitute, list(e), env = parent.frame())
      dimnames(a) <- list(NULL, deparse(e))
      return(a)
    }
  )
}

#' @rdname array-apply
#' @title Array function application
#' @param a An array.
#' @param axis The axis or axes along the function \code{FUN} is applied. Per default {\code{NULL}}, each element is passed separately as an argument to the function. Otherwise, the elements along \code{axis} are passed in combination.
#' @param FUN The function to be applied on each element of \code{a}.
#' @param ... Optional arguments passed to \code{FUN}.
#' @param simplify A logical indicating whether results should be simplified if possible, default \code{TRUE}.
#'
#' @details This function is a wrapper for \code{\link{apply}}. The functions \code{as.marray_int()}, \code{as.marray_dbl()}, \code{as.marray_raw()}, \code{as.marray_cpx()}, \code{as.marray_chr()} and \code{as.marray_lgl()}
#' transform the elements of an array into its corresponding types \code{\link{integer}}, \code{\link{double}}, \code{\link{raw}}, \code{\link{complex}}, \code{\link{character}} and \code{\link{logical}}.
#'
#' @return The array \code{a} with elements processed by \code{FUN}.
#'
#' @examples
#' a <- marray(seq(24), dim = c(4, 3, 2))
#' foreach.array(a, FUN = function(x) 3 * x)
#' as.marray_int(a)
#' as.marray_dbl(a)
#' as.marray_raw(a)
#' as.marray_chr(a)
#' as.marray_lgl(a)
#' as.marray_norm(a)
#'
#' for (i in seq(3)) { print(foreach.array(a, axis = i, FUN = sum)) }
#'
#' @export
foreach.array <- function(a, axis = NULL, FUN, ..., simplify = TRUE) {
  if (!missing(FUN)) FUN <- match.fun(FUN) else return(a)
  a <- .standardize_array(a)
  axis <- if (is.null(axis)) seq_along(DIM(a)) else .standardize_axis(axis, ndim(a))
  apply(a, MARGIN = axis, FUN = FUN, ..., simplify = simplify)
}

#' @rdname array-apply
#' @description Applies a function over or along axes of an array.
#'
#' @param axes Axes over which function is applied.
#'
#' @details This function corresponds to \code{apply_over_axes()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.apply_over_axes.html}{see}).\cr
#' If a function is applied over or along axes, the function is called for those axes. The answer of the function calls relates to the remaining axes. For e.g.,
#' if the sum of rows (first axis) for a 2D-array is calculated, the function must be applied over the columns (second axis) to get the sum for each row.
#'
#' @return The output array.
#'
#' @examples
#' a <- marray(0:23, dim = c(2, 3, 4))
#' apply_over_axes(a, axes = c(1, 3), FUN = sum)
#'
#' @export
apply_over_axes <- function(a, axes = NULL, FUN, ..., simplify = TRUE) {
  if (!missing(FUN)) FUN <- match.fun(FUN) else return(a)
  a <- .standardize_array(a)
  d <- DIM(a)
  ds <- seq_along(d)
  axes <- if (is.null(axes)) ds else .standardize_axis(axes, length(d))
  ds.call <- ds[-axes]
  apply(a, MARGIN = ds.call, FUN = FUN, ..., simplify = simplify)
}

#' @rdname array-apply
#' @export
as.marray_int <- function(a) { class(a) <- "integer"; a } # foreach.array(a, FUN = as.integer)

#' @rdname array-apply
#' @export
as.marray_dbl <- function(a) { class(a) <- "double"; a } # foreach.array(a, FUN = as.double)

#' @rdname array-apply
#' @export
as.marray_raw <- function(a) { class(a) <- "raw"; a } # foreach.array(a, FUN = as.raw)

#' @rdname array-apply
#' @export
as.marray_cpx <- function(a) { class(a) <- "complex"; a } # foreach.array(a, FUN = as.complex)

#' @rdname array-apply
#' @export
as.marray_chr <- function(a) { class(a) <- "character"; a } # foreach.array(a, FUN = as.character)

#' @rdname array-apply
#' @export
as.marray_lgl <- function(a) { class(a) <- "logical"; a } # foreach.array(a, FUN = as.logical)

#' @rdname array-apply
#' @param mean Vector of means.
#' @param sd Vector of standard deviations.
#' @param log A logical value indicating whether the probabilities are given as \code{\link{log}}.
#' @export
as.marray_norm <- function(a, mean = 0, sd = 1, log = FALSE) {
  a <- .standardize_array(a)
  if (is.null(mean)) mean <- mean(a)
  if (is.null(sd)) sd <- sd(a)
  foreach.array(a, FUN = dnorm, mean = mean, sd = sd, log = log)
}
