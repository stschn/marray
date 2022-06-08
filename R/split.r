#' @title Array splitting
#' @description Split an array into multiple sub-arrays along an axis.
#'
#' @param a An array to be divided into sub-arrays.
#' @param indices_or_sections An integerish vector indicating the type of splitting.
#'   A single integer, N, denotes that the array \code{a} will be divided into N arrays along \code{axis}.
#'   A sorted vector of integers denotes the entries where along \code{axis} the array \code{a} is split.
#' @param axis The axis along which to split, default is \code{1}.
#'
#' @details This function corresponds to \code{array_split()} from NumPy.
#'
#' @return A list of sub-arrays.
#'
#' @examples
#' a <- marray(1:8)
#' array_split(a, indices_or_sections = 3)
#' array_split(a, indices_or_sections = c(3, 5, 6))
#'
#' a <- marray(1:12, dim = c(4, 3))
#' array_split(a, indices_or_sections = 2)
#' array_split(a, indices_or_sections = 2, axis = 2L)
#'
#' a <- marray(1:24, dim = c(4, 3, 2), order = "F")
#' array_split(a, indices_or_sections = 2)
#' array_split(a, indices_or_sections = 2, axis = 2L)
#'
#' @export
array_split <- function(a, indices_or_sections, axis = 1L) {
  d <- DIM(a)
  nd <- length(d)
  axis[which((axis < 0L) | (axis > nd))] <- nd
  indices_or_sections <- sort(indices_or_sections)

  ntotal <- d[axis]
  if (length(indices_or_sections) == 1L)
    indices_or_sections <- seq(ceiling(ntotal / indices_or_sections) -> section_size, ntotal, section_size)
  indices_or_sections <- c(1L, indices_or_sections)
  x <- seq_len(ntotal)
  sections <- unname(split(x, findInterval(x, indices_or_sections[-1L] + 1L)))
  stopifnot("number sections must be larger than 0." = length(sections) > 0L)

  sub_arys <- vector(mode = "list", length = length(sections))
  axes <- lapply(d, seq_len)
  for (i in seq_along(sections)) {
    axes[[axis]] <- sections[[i]]
    sub_arys[[i]] <- slice(a, axes)
  }
  sub_arys
}

#' @title Array splitting
#' @description Split an array into multiple sub-arrays vertically (row-wise).
#'
#' @param a An array to be divided into sub-arrays.
#' @param indices_or_sections An integerish vector indicating the type of splitting.
#'   A single integer, N, denotes that the array \code{a} will be divided into N arrays along \code{axis}.
#'   A sorted vector of integers denotes the entries where along \code{axis} the array \code{a} is split.
#'
#' @details This function corresponds to \code{vsplit()} from NumPy.
#'   It's equivalent to \code{\link{array_split}} along the first axis.
#'
#' @return A list of sub-arrays.
#'
#' @export
vsplit <- function(a, indices_or_sections) {
  array_split(atleast_1d(a), indices_or_sections = indices_or_sections, axis = 1L)
}

#' @title Array splitting
#' @description Split an array into multiple sub-arrays horizontally (column-wise).
#'
#' @param a An array to be divided into sub-arrays.
#' @param indices_or_sections An integerish vector indicating the type of splitting.
#'   A single integer, N, denotes that the array \code{a} will be divided into N arrays along \code{axis}.
#'   A sorted vector of integers denotes the entries where along \code{axis} the array \code{a} is split.
#'
#' @details This function corresponds to \code{hsplit()} from NumPy.
#'   It's equivalent to \code{\link{array_split}} along the second axis.
#'
#' @return A list of sub-arrays.
#'
#' @export
hsplit <- function(a, indices_or_sections) {
  array_split(atleast_2d(a), indices_or_sections = indices_or_sections, axis = 2L)
}

#' @title Array splitting
#' @description Split an array into multiple sub-arrays along the 3rd axis (depth-wise).
#'
#' @param a An array to be divided into sub-arrays.
#' @param indices_or_sections An integerish vector indicating the type of splitting.
#'   A single integer, N, denotes that the array \code{a} will be divided into N arrays along \code{axis}.
#'   A sorted vector of integers denotes the entries where along \code{axis} the array \code{a} is split.
#'
#' @details This function corresponds to \code{dsplit()} from NumPy.
#'   It's equivalent to \code{\link{array_split}} along the third axis.
#'
#' @return A list of sub-arrays.
#'
#' @export
dsplit <- function(a, indices_or_sections) {
  array_split(atleast_3d(a), indices_or_sections = indices_or_sections, axis = 3L)
}
