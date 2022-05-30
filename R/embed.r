#' @title Series embedding
#'
#' @param data The data to be embedded into a shifted series.
#' @param length The length of a series.
#' @param flip A logical value indicating whether the embedded series should be reversed.
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
embedseries <- function(data, length, flip) {
  UseMethod("embedseries")
}

#' @rdname embedseries
#' @export
embedseries.default <- function(data, length = 1L, flip = TRUE) {
  length <- ifelse(is.null(length) || (length < 1L), 1L, length) # at least a length of 1 is needed
  a <- stats::embed(as.vector(flatten(data)), dimension = length)
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
