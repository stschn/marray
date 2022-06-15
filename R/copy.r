#' @title Array copying
#' @description Copy values from one array to another.
#'
#' @param dst The array to which values are copied.
#' @param src The array from which values are copied.
#' @param dst_axis_index Axis indexing instructions for \code{dst}. Specifies the area in \code{dst} to which values are copied.
#' @param src_axis_index Axis indexing instructions for \code{src}. Specifies the area in \code{src} from which values are copied.
#'
#' @details This function corresponds partially to \code{copyto()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.copyto.html}{see}).
#'
#' @return The array \code{dst}.
#'
#' @examples
#' a <- zeros(dim = c(4, 3, 2))
#' b <- ones(dim = c(3, 3))
#' copyto(a, b, dst_axis_index = list(i = 2:3, j = 1:2), src_axis_index = list(i = 1:2, j = 2:3))
#'
#' @export
copyto <- function(dst, src, dst_axis_index = NULL, src_axis_index = NULL) {
  slice(dst, dst_axis_index) <- slice(src, src_axis_index)
  dst
}
