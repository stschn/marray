#' @title Array binding
#' @description Combine arrays along a specified dimension.
#'
#' @param ... Any number of objects that are combined into an array. The objects can be packed into a \code{list}. The dimensions of these objects must be equal, excluding axis, if they are not to be coerced into a certain \code{input_shape}.
#' @param input_shape The dimension the input objects are to be coerced. By default \code{NULL}, the original dimensions are used.
#' @param axis The dimension along the objects are combined. By default (\code{-1}), the last dimension is used for binding the arrays.
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
#' @export
mabind <- function(..., input_shape = NULL, axis = -1, order = c("C", "F")) {
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
  if ((axis <= 0L) || (axis > N)) axis <- N

  # Coerce all arguments to have the same number of dimensions (by adding one, if necessary)
  # and permute them to put the join dimension (axis) last.
  all_dims <- lapply(arys, dim)
  all_dims <- lapply(all_dims, function(d) {
    if (length(d) < N) d <- append(d, 1L, axis - 1L)
    d
  })

  # Construct matrix of dimensions
  # Rows are equal to the length of the dimension(s) and Cols are equal to the number of arrays
  all_dims <- sapply(all_dims, identity)
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
