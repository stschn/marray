#' @rdname maskedarray
#' @title Masked Array
#' @description Create a binary array based on meeting conditions.
#'
#' @param a An array.
#' @param condition Logical expression indicating elements to search for.
#'
#' @return A binary array with the shape of \code{a}. If an element in \code{a} meets \code{condition} the result value is one at the corresponding position, otherwise it is zero.
#' @examples
#' a <- marray(1:24, dim = c(4, 3, 2))
#' memberof(a, condition = a %in% c(1, 2, 15, 25, 11, 23))
#' memberof(a, condition = (a > 11) & (a <= 23))
#'
#' @seealso \code{\link{where}}.
#'
#' @export
memberof <- function(a, condition) {
  return(where(a, condition = condition, true = 1, false = 0))
}

#' @rdname maskedarray
#' @description Create a binary array in rectangular form based on center coordinates and window sizes.
#'
#' @param dim Shape of the masked array.
#' @param ... Indexing instructions with or without letters in form of \code{name = value} pairs. The names of the arguments specify the axis and the values its positions.
#' @param size An integerish vector or matrix indicating the size of the window around the center coordinates given by \code{...}.
#'
#' @details
#' The indexing instructions must correspond to the number of axis within \code{dim}. For every axis an integerish vector must be specified which can be used as center coordinates.
#' The \code{size} argument is either a single value used for every axis, or a vector, or a matrix. If a vector or matrix is given, the number of elements respectively
#' the number of columns must be also identical with the number of axis.
#'
#' @return A binary array with shape \code{dim}. All elements within the window around each center coordinate have the value one, the others zero.
#' @examples
#' m <- mask_rectangle(dim = c(30, 30, 10), i = c(11, 23), j = c(15, 20), k = c(4, 7), size = 3)
#' slice(m, i = c(9:13), j = c(13:17), k = c(2:6))
#'
#' @export
mask_rectangle <- function(dim, ..., size) {
  if (missing(...)) return(zeros(dim))
  a <- zeros(dim)
  n <- length(dim)

  center <- .aindex(dim, ...)
  stopifnot("Indexing instructions for center coordinates must be equal to the number of dimensions." = length(center) == n,
            "Indexing instructions must be of equal length for all axes." = length(unique(lengths(center))) == 1L)
  center <- marray(unlist(center) -> center, dim = c(length(center) / n, n), order = "F")

  if (length(size) == 1L) size <- rep(size, n)
  if (!identical(DIM(center), DIM(size)))
    size <- tile(as.array(size), reps = c(NROW(center), 1L))

  offset <- as.marray_int(size %% 2 == 0) # is size even?
  size <- floor(size / 2)
  lower <- center - (size - offset)
  upper <- center + size

  lower[lower < 1] <- 1L
  upper <- t(apply(upper, 1L, function(row) {
    i <- row > dim
    row[i] <- dim[i]
    row }))

  idx <- lapply(seq_len(NROW(lower)), function(i) {
    l <- lapply(seq_len(NCOL(lower)), function(j) {
      seq.int(lower[i, j], upper[i, j])
    })
    l
  })
  for (i in seq_along(idx))
    slice(a, idx[[i]]) <- 1L
  a
}

#' @rdname maskedarray
#' @description Create a binary array in circular form based on coordinates and radiuses.
#'
#' @param radius An integerish vector indicating the radius around the coordinates given by \code{...}. A single value will be used for every \code{axis}.
#'
#' @return A binary array with shape \code{dim}. All elements within the radius around of each coordinate have the value one, the others zero.
#' @examples
#' m <- mask_circle(dim = c(10, 10), i = 4, j = 4, radius = 3)
#' m
#'
#' @export
mask_circle <- function(dim, ..., radius) {
  if (missing(...)) return(zeros(dim))
  a <- zeros(dim)
  n <- length(dim)

  center <- .aindex(dim, ...)
  stopifnot("Indexing instructions for center coordinates must be equal to the number of dimensions." = length(center) == n,
            "Indexing instructions must be of equal length for all axes." = length(unique(lengths(center))) == 1L)
  center <- marray(unlist(center) -> center, dim = c(length(center) / n, n), order = "F")

  if (length(radius) == 1L) radius <- rep(radius, NROW(center))
    size <- tile(expand_dims(as.array(radius)), reps = length(dim))

  # A circular form is inside a rectangular form with length of radius around a center coordinate
  offset <- as.marray_int(size %% 2 == 0) # is size even?
  lower <- center - (size - offset)
  upper <- center + size

  lower[lower < 1] <- 1L
  upper <- t(apply(upper, 1L, function(row) {
    i <- row > dim
    row[i] <- dim[i]
    row }))

  idx <- lapply(seq_len(NROW(lower)), function(i) {
    l <- lapply(seq_len(NCOL(lower)), function(j) {
      seq.int(lower[i, j], upper[i, j])
    })
    l
  })

  for (i in seq_along(idx)) {
    # Create coordinates for circular form
    axes <- expand.grid(idx[[i]])
    names(axes) <- (.axis_symbol(seq(n)) -> aidx)
    # Compute distance-to-centre
    axes$d2 <- sqrt(apply(mapply(function(x, y) (x-y)^2L, axes, y = center[i, ]), 1L, sum))
    # Compare with circle radius
    axes$inside <- axes$d2 <= radius[i]
    # Get indices of elements that are inside the circle radius
    aidx <- as.matrix(axes[which(axes$inside), aidx])
    for (j in seq_len(NROW(aidx)))
      slice(a, as.list(aidx[j, ])) <- 1
  }
  a
}
