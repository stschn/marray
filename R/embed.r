#' @title Series embedding
#'
#' @param data The data to be embedded into a shifted series.
#' @param length The length of a series.
#' @param flip A logical value indicating whether the embedded series should be reversed.
#' @param order The order in which elements of \code{data} should be read during flattening.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @return An ongoing shifted series of \code{data}.
#' @export
#'
#' @examples
#' x <- seq_len(1e+6)
#' df <- data.frame(a = seq_len(1e+6), b = -seq_len(1e+6))
#' a <- embedseries(x, length = 11L)
#' head(a)
#' a <- embedseries(df, length = 23L)
#' head(a)
embedseries <- function(data, ...) {
  UseMethod("embedseries")
}

#' @rdname embedseries
#' @export
embedseries.default <- function(data, length = 1L, flip = TRUE, order = c("C", "F")) {
  length <- ifelse(is.null(length) || (length < 1L), 1L, length) # at least a length of 1 is needed
  a <- stats::embed(as.vector(flatten(data, order = order)), dimension = length)
  if (flip) a <- flip(a, axis = 2L)
  a
}

#' @rdname embedseries
#' @export
embedseries.matrix <- function(data, length = 1L, flip = TRUE) {
  length <- ifelse(is.null(length) || (length < 1L), 1L, length)
  n <- NROW(data) - length + 1L
  m <- NCOL(data)
  a <- empty(dim = c(n, length, m))
  if (flip)
    for (i in seq_len(m)) a[, , i] <- flip(stats::embed(data[, i], dimension = length), axis = 2L)
  else
    for (i in seq_len(m)) a[, , i] <- stats::embed(data[, i], dimension = length)
  dimnames(a) <- list(NULL, NULL, colnames(data))
  a
}

#' @rdname embedseries
#' @export
embedseries.data.frame <- function(data, length = 1L, flip = TRUE) {
  return(embedseries.matrix(as.matrix(data), length, flip))
}
