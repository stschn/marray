#' @title Array stack
#' @description Stack 1D arrays as columns into a 2D array.
#'
#' @param ... Any numbers of objects they are coerced to 1D arrays. The objects can be packed into a \code{list}. The length of these objects must be equal.
#' @param order The order in which elements of the objects should be read during coercing to arrays.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds to \code{column_stack()} from NumPy.
#' @return An 2D array formed by stacking all input arrays.
#'
#' @examples
#' a <- 1:3
#' b <- 4:6
#' column_stack(a, b)
#' @export
column_stack <- function(..., order = c("C", "F")) {
  order <- match.arg(order)
  list_of_arrays <- list(...)
  # If arrays are coerced into a list like list(a1, a2, a3, ...), flat arguments into a simple list
  if (any(sapply(list_of_arrays, is.list)))
    list_of_arrays <- unlist(list_of_arrays, recursive = FALSE)

  # Transform objects to 1D arrays
  list_of_arrays <- lapply(list_of_arrays, FUN = flatten, order = order)

  # Check equality of vector sizes
  if (length(unique(sapply(list_of_arrays, length))) != 1L)
    stop("All input arrays must have the same length (number of elements).", call. = FALSE)

  # Construct 2D array (matrix)
  #matrix(unlist(list_of_arrays), ncol = length(list_of_arrays))
  do.call(cbind, list_of_arrays)
}

#' @title Array stack
#' @description Stack 1D arrays as rows into a 2D array.
#'
#' @param ... Any numbers of objects they are coerced to 1D arrays. The objects can be packed into a \code{list}. The length of these objects must be equal.
#' @param order The order in which elements of the objects should be read during coercing to arrays.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds to \code{row_stack()} from NumPy.
#' @return An 2D array formed by stacking all input arrays.
#'
#' @examples
#' a <- 1:3
#' b <- 4:6
#' row_stack(a, b)
#' @export
row_stack <- function(..., order = c("C", "F")) {
  order <- match.arg(order)
  list_of_arrays <- list(...)
  # If arrays are coerced into a list like list(a1, a2, a3, ...), flat arguments into a simple list
  if (any(sapply(list_of_arrays, is.list)))
    list_of_arrays <- unlist(list_of_arrays, recursive = FALSE)

  # Transform objects to 1D arrays
  list_of_arrays <- lapply(list_of_arrays, FUN = flatten, order = order)

  # Check equality of vector sizes
  if (length(unique(sapply(list_of_arrays, length))) != 1L)
    stop("All input arrays must have the same length (number of elements).", call. = FALSE)

  # Construct 2D array (matrix)
  #matrix(unlist(list_of_arrays), nrow = length(list_of_arrays), byrow = TRUE)
  do.call(rbind, list_of_arrays)
}
