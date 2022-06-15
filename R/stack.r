#' @title Array stack
#' @description Stack arrays in sequence vertically (row-wise).
#'
#' @param ... Any number of objects that are combined into an array. The objects can be packed into a \code{list}. The dimensions of these objects must be equal, excluding axis 1, if they are not to be coerced into a certain \code{input_shape}.
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
  mabind(..., input_shape = input_shape, axis = 1L, order = order)
}

#' @title Array stack
#' @description Stack arrays in sequence horizontally (column-wise).
#'
#' @param ... Any number of objects that are combined into an array. The objects can be packed into a \code{list}. The dimensions of these objects must be equal, excluding axis 2, if they are not to be coerced into a certain \code{input_shape}.
#' @param input_shape The dimension the input objects are to be coerced. By default \code{NULL}, the original dimensions are used.
#' @param order The order in which elements of the objects should be read during coercing to arrays.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds to \code{hstack()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.hstack.html}{see}).
#'   It's equivalent to \code{\link{mabind}} along the second axis.
#'
#' @return The array formed by stacking the given arrays.
#'
#' @export
hstack <- function(..., input_shape = NULL, order = c("C", "F")) {
  mabind(atleast_2d(..., order = order), input_shape = input_shape, axis = 2L, order = order)
}

#' @title Array stack
#' @description Stack arrays in sequence along the 3rd axis (depth-wise).
#'
#' @param ... Any number of objects that are combined into an array. The objects can be packed into a \code{list}. The dimensions of these objects must be equal, excluding axis 2, if they are not to be coerced into a certain \code{input_shape}.
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
  mabind(atleast_3d(..., order = order), input_shape = input_shape, axis = 3L, order = order)
}

#' @title Array stack
#' @description Stack 1-D arrays as columns into a 2-D array.
#'
#' @param ... Any numbers of objects they are coerced to 1-D arrays. The objects can be packed into a \code{list}. The length of these objects must be equal.
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
  list_of_arrays <- .dots(...)

  # Transform objects to 1-D arrays
  list_of_arrays <- lapply(list_of_arrays, FUN = flatten, order = order)

  # Check equality of vector sizes
  if (length(unique(sapply(list_of_arrays, length))) != 1L)
    stop("all input arrays must have the same length (number of elements).", call. = FALSE)

  # Construct 2-D array (matrix)
  #matrix(unlist(list_of_arrays), ncol = length(list_of_arrays))
  do.call(cbind, list_of_arrays)
}

#' @title Array stack
#' @description Stack 1-D arrays as rows into a 2-D array.
#'
#' @param ... Any numbers of objects they are coerced to 1-D arrays. The objects can be packed into a \code{list}. The length of these objects must be equal.
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
  list_of_arrays <- .dots(...)

  # Transform objects to 1-D arrays
  list_of_arrays <- lapply(list_of_arrays, FUN = flatten, order = order)

  # Check equality of vector sizes
  if (length(unique(sapply(list_of_arrays, length))) != 1L)
    stop("all input arrays must have the same length (number of elements).", call. = FALSE)

  # Construct 2-D array (matrix)
  #matrix(unlist(list_of_arrays), nrow = length(list_of_arrays), byrow = TRUE)
  do.call(rbind, list_of_arrays)
}
