#' @title Array sliding
#' @description Slides over an array with a window of given size and given stride.
#'
#' @param a An array.
#' @param size An integerish vector indicating the size of the window sliding over \code{a}. A single value will be used for every \code{axis}.
#' @param stride An integerish vector specifying the amount of the window shift per each single move. A single value will be used for every \code{axis}.
#' @param axis The axis or axes along which to slide over \code{a}, default are all axes within the dimension space (shape). The not-considered axes are used without any limitation.
#'
#' @return A list of windowed sub-arrays.
#'
#' @examples
#' a <- marray(sample(7 * 4 * 2), dim = c(7, 4, 2))
#' arys <- slide(a) # sliding along each axes with a size of 1 and a stride of 1
#' arys <- slide(a, size = c(2, 1), axis = c(1, 2)) # sliding along first and second axes with sizes of 2 and 1 and a stride of 1 for both axes
#'
#' @export
slide <- function(a, size = 1L, stride = 1L, axis = NULL) {
  d <- DIM(a)
  nd <- length(d)

  if (is.null(axis)) axis <- seq_len(nd)
  axis <- .standardize_axis(axis, nd)
  keep <- setdiff(seq_along(d), axis)

  if (length(size) == 1L) size <- rep(size, length(axis))
  if (length(stride) == 1L) stride <- rep(stride, length(axis))
  stopifnot("size must be of the same length as axis." = length(size) == length(axis),
            "stride must be of the same length as axis." = length(stride) == length(axis),
            "a size value can not be greater than the corresponding axis." = all(size <= d[axis]),
            "a stride value can not be greater than the corresponding axis." = all(stride <= d[axis]))

  s <- vector("integer", nd)
  s[axis] <- size
  s[keep] <- d[keep]
  size <- s

  s <- vector("integer", nd)
  s[axis] <- stride
  s[keep] <- 1L
  stride <- s

  ary_indices <- lapply(seq_along(d), function(i) {
    l <- as.list(as.data.frame(t(.broadcast(d[i], size[i], stride[i]))))
    names(l) <- NULL
    l
  })

  df <- expand.grid(ary_indices)
  ary_indices <- lapply(apply(df, 1L, identity), identity)
  ary_indices <- lapply(ary_indices, function(idx) { names(idx) <- NULL; idx })
  subarrays <- lapply(ary_indices, function(idx) slice(a, idx))
  subarrays
}

# helper function
# Given the length of an axis, the result is a matrix with the indices for selecting a sized sub-range considering a stride
.broadcast <- function(xlen, size = 1L, stride = 1L) {
  nrows <- floor((xlen - size) / stride) + 1L
  outer(stride * seq_len(nrows), seq_len(size), FUN = "+") - stride
}
