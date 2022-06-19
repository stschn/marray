#' @title Array repetition
#'
#' @param a The input array.
#' @param reps Number of repetitions of \code{a} along each axis.
#' 
#' @details This function corresponds to \code{tile()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.tile.html}{see}).
#'
#' @return The tiled output array.
#' 
#' @examples
#' a <- marray(c(0, 1, 2))
#' tile(a, 3)
#' tile(a, c(2, 2))
#' tile(a, c(2, 1, 2))
#' 
#' a <- marray(1:4, dim = c(2, 2))
#' tile(a, 2)
#' tile(a, c(2, 1))
#'
#' @export
tile <- function(a, reps) {
  reps_len <- length(reps)
  a <- ndmin(a, n = reps_len, axis = 1L)
  if (all(reps == 1L)) return(a)
  
  d <- DIM(a)
  nd <- length(d)
  
  if (reps_len < nd)
    reps <- c(rep(1L, nd - reps_len), reps)
  newdim <- d * reps
  
  n <- nsize(a)
  if (n > 0)
    for (i in seq_along(d)) {
      nrep <- reps[i]
      if (nrep != 1) {
        remaining_len <- as.integer(nsize(a) / n)
        a <- reshape.array(a, dim = c(remaining_len, n))
        arys <- lapply(seq_len(nrep), function(i) a )
        a <- mabind(arys)
      }
      n <- floor(n / d[i])
    }
  reshape.array(a, dim = newdim)
}