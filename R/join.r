#' @title Array binding
#' @description Combine arrays along a specified dimension.
#'
#' @param ... Any number of objects that are combined into an array. The objects can be packed into a \code{\link{list}}. The dimensions of these objects must be equal, excluding axis, if they are not to be coerced into a certain \code{input_shape}.
#' @param axis The dimension along the objects are combined. By default (\code{-1}), the last dimension is used for binding the arrays.
#' @param input_shape The dimension the input objects are to be coerced. By default \code{NULL}, the original dimensions are used.
#' @param order The order in which elements of the objects should be read during coercing to arrays.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds to \code{concatenate()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.concatenate.html}{see}).
#' @return An array as a combination of all input arrays along a specified dimension.
#'
#' @examples
#' a <- marray(1:11)
#' b <- marray(2:23)
#' mabind(a, b)
#'
#' a <- marray(1:12, dim = c(4, 3))
#' b <- marray(1:12, dim = c(4, 3, 1))
#' mabind(a, b)
#'
#' a1 <- marray(1:24, dim = c(4, 3, 2), order = "F"); a1
#' a2 <- marray(-1:-24, dim = c(4, 3, 2), order = "F"); a2
#' a3 <- marray(sample(24), dim = c(4, 3, 2), order = "F"); a3
#' mabind(a1, a2, a3) # output is an 4x3x6 array
#' mabind(a1, a2, a3, axis = 1) # output is an 12x3x2 array
#' mabind(a1, a2, a3, axis = 2) # output is an 4x9x2 array
#'
#' a <- marray(sample(12), dim = c(4, 3))
#' b <- marray(sample(12), dim = c(4, 3, 1))
#' d <- marray(1:12)
#' e <- marray(sample(12), dim = c(4, 3, 1))
#' f <- marray(sample(4*3*1*1), dim = c(4, 3, 1, 1))
#' mabind(a, b, d, e, f)
#'
#' @export
mabind <- function(..., axis = -1, input_shape = NULL, order = c("C", "F")) {
  order <- match.arg(order)
  arys <- .dots(...)
  # Discard empty elements
  if (any(discard <- sapply(arys, FUN = is.null)))
    arys <- arys[!discard]
  if (length(arys) == 0L)
    return(NULL)

  # Transform objects to arrays
  arys <- lapply(arys, FUN = marray, dim = input_shape, order = order)
  N <- max(1, sapply(arys, FUN = ndim))
  axis <- .standardize_axis(axis, N)

  # Coerce all arguments to have the same number of dimensions (by adding one, if necessary)
  # and permute them to put the join dimension (axis) last.
  cdim <- .coerce_dim(arys, axis)
  arys <- lapply(arys, function(a) {
    #while (ndim(a) < (N - 1L)) a <- expand_dims(a)
    if (ndim(a) < (N - 1L)) a <- marray(a, dim = cdim)
    if (ndim(a) < N) a <- expand_dims(a, axis)
    a
  })

  # Construct matrix of dimensions
  # Rows are equal to the length of the dimension(s) and Cols are equal to the number of arrays
  all_dims <- sapply(arys, FUN = dim)
  if (is.vector(all_dims)) all_dims <- t(all_dims)
  if (!(is.matrix(all_dims) && all(apply(all_dims[-axis, , drop = FALSE], 1L, function(x) length(unique(x)) == 1L) == TRUE)))
    stop("All input arrays must have the same shape (number of dimensions), excluding axis.", call. = FALSE)

  perm <- seq_len(N)
  #if (!(axis < 0)) perm <- as.integer(append(perm[!perm %in% axis], axis)) # put axis last
  if (!(axis == N)) perm <- as.integer(c(perm[-axis], axis)) # put axis last

  # Adopt dimensions of arrays, if necessary
  if (any(perm != seq_along(perm)))
    arys <- lapply(arys, FUN = transpose, perm)

  # Construct output array
  out <- array(unlist(arys), dim = c(all_dims[-axis, 1L], sum(all_dims[axis, ])))
  # Permute the output array to put the join dimension back in the right place
  if (any(order(perm) != seq_along(perm)))
    out <- transpose(out, order(perm))
  out
}

# helper function
# Retrieve the dimension arrays must be coerced if they do not have the highest number of dimensions and size
.coerce_dim <- function(arys, axis) {
  N <- max(1L, sapply(arys, ndim))
  nd <- which(sapply(arys, ndim) == N)
  ns <- which((s <- sapply(arys, nsize)) == max(s))

  i <- intersect(nd, ns) # for more than two arguments: Reduce(intersect, list(nd, ns, ...))
  if (length(i) == 0L)
    stop("index mismatch between arrays number of dimensions and sizes.", call. = FALSE)
  dima <- dim(arys[[i[1L]]]) # dima <- lapply(arys, dim)[[i[1L]]]
  if (length(dima) > 1L) dima <- dima[-axis]
  dima
}

#' @title Array stack
#' @description Stack arrays in sequence vertically (row-wise).
#'
#' @param ... Any number of objects that are combined into an array. The objects can be packed into a \code{\link{list}}. The dimensions of these objects must be equal, excluding axis 1, if they are not to be coerced into a certain \code{input_shape}.
#' @param input_shape The dimension the input objects are to be coerced. By default \code{NULL}, the original dimensions are used.
#' @param order The order in which elements of the objects should be read during coercing to arrays.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds to \code{vstack()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.vstack.html}{see}).
#'   It's equivalent to \code{\link{mabind}} along the first axis.
#'
#' @return The array formed by stacking the given arrays.
#'
#' @export
vstack <- function(..., input_shape = NULL, order = c("C", "F")) {
  mabind(atleast_2d(..., order = order), axis = 1L, input_shape = input_shape, order = order)
}

#' @title Array stack
#' @description Stack arrays in sequence horizontally (column-wise).
#'
#' @param ... Any number of objects that are combined into an array. The objects can be packed into a \code{\link{list}}. The dimensions of these objects must be equal, excluding axis 2, if they are not to be coerced into a certain \code{input_shape}.
#' @param input_shape The dimension the input objects are to be coerced. By default \code{NULL}, the original dimensions are used.
#' @param order The order in which elements of the objects should be read during coercing to arrays.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds to \code{hstack()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.hstack.html}{see}).
#'   It's equivalent to \code{\link{mabind}} along the second axis, except for 1-D arrays where it concatenates along the first axis.
#'
#' @return The array formed by stacking the given arrays.
#'
#' @export
hstack <- function(..., input_shape = NULL, order = c("C", "F")) {
  arrs <- atleast_1d(..., order = order)
  if (!is.list(arrs)) return(arrs)
  # As a special case, dimension 1 of 1-D arrays is horizontal
  if (all(sapply(arrs, ndim) == 1L))
    mabind(arrs, axis = 1L, order = order)
  else
    mabind(arrs, axis = 2L, input_shape = input_shape, order = order)
}

#' @title Array stack
#' @description Stack arrays in sequence along the 3rd axis (depth-wise).
#'
#' @param ... Any number of objects that are combined into an array. The objects can be packed into a \code{\link{list}}. The dimensions of these objects must be equal, excluding axis 2, if they are not to be coerced into a certain \code{input_shape}.
#' @param input_shape The dimension the input objects are to be coerced. By default \code{NULL}, the original dimensions are used.
#' @param order The order in which elements of the objects should be read during coercing to arrays.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds to \code{dstack()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.dstack.html}{see}).
#'   It's equivalent to \code{\link{mabind}} along the third axis.
#'
#' @return The array formed by stacking the given arrays.
#'
#' @export
dstack <- function(..., input_shape = NULL, order = c("C", "F")) {
  mabind(atleast_3d(..., order = order), axis = 3L, input_shape = input_shape, order = order)
}

#' @title Array stack
#' @description Stack 1-D arrays as columns into a 2-D array.
#'
#' @param ... Any numbers of objects they are coerced to 1-D arrays. The objects can be packed into a \code{\link{list}}. The length of these objects must be equal.
#' @param order The order in which elements of the objects should be read during coercing to arrays.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds to \code{column_stack()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.column_stack.html}{see}).
#' @return An 2-D array formed by stacking all input arrays.
#'
#' @examples
#' a <- 1:3
#' b <- 4:6
#' column_stack(a, b)
#' @export
column_stack <- function(..., order = c("C", "F")) {
  order <- match.arg(order)
  arys <- .dots(...)

  # Transform objects to 1-D arrays
  arys <- lapply(arys, FUN = flatten, order = order)

  # Check equality of vector sizes
  if (length(unique(sapply(arys, length))) != 1L)
    stop("all input arrays must have the same length (number of elements).", call. = FALSE)

  # Construct 2-D array (matrix)
  #matrix(unlist(arys), ncol = length(arys))
  do.call(cbind, arys)
}

#' @title Array stack
#' @description Stack 1-D arrays as rows into a 2-D array.
#'
#' @param ... Any numbers of objects they are coerced to 1-D arrays. The objects can be packed into a \code{\link{list}}. The length of these objects must be equal.
#' @param order The order in which elements of the objects should be read during coercing to arrays.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds to \code{row_stack()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.row_stack.html}{see}).
#' @return An 2-D array formed by stacking all input arrays.
#'
#' @examples
#' a <- 1:3
#' b <- 4:6
#' row_stack(a, b)
#' @export
row_stack <- function(..., order = c("C", "F")) {
  order <- match.arg(order)
  arys <- .dots(...)

  # Transform objects to 1-D arrays
  arys <- lapply(arys, FUN = flatten, order = order)

  # Check equality of vector sizes
  if (length(unique(sapply(arys, length))) != 1L)
    stop("all input arrays must have the same length (number of elements).", call. = FALSE)

  # Construct 2-D array (matrix)
  #matrix(unlist(arys), nrow = length(arys), byrow = TRUE)
  do.call(rbind, arys)
}
