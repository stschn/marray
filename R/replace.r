#' @title Array searching
#' @description Rescale elements of an array.
#'
#' @param a An array.
#' @param ... Indexing instructions with or without letters in form of \code{name = value} pairs. The names of the arguments specify the axis and the values its positions.
#' @param FUN Rescale function.
#' @return An array with rescaled values.
#'
#' @examples
#' a <- marray(seq(24), dim = c(4, 3, 2))
#' rescale.array(a, i = c(1, 3), j = 2, FUN = \(x) { x * 3 })
#'
#' @export
rescale.array <- function(a, ..., FUN) {
  a <- .standardize_array(a)
  if (missing(FUN)) return(a)
  slice(a, ...) <- FUN(slice(a, ...))
  a
}

#' @title Array searching
#' @description Replace elements of an array with new values.
#'
#' @param a An array.
#' @param ... Indexing instructions with or without letters in form of \code{name = value} pairs. The names of the arguments specify the axis and the values its positions.
#' @param oldvalue A vector of values to search for.
#' @param newvalue A vector of values to replace \code{oldvalue}.
#'   A single value is used as a replacement for all values in \code{oldvalue}. Otherwise, the length of \code{newvalue} must be equal to the length of \code{oldvalue}.
#'   In that case a value in \code{newvalue} at a certain position is used to replace the value in \code{oldvalue} at the same position.
#'
#' @details This function is an equivalent to \code{place()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.place.html}{see}).
#'   In opposite to NumPy this function is more precise when replacing values.
#'
#' @return An array with replaced values.
#'
#' @examples
#' a <- marray(c(rep(1, 4), 2, rep(3, 5), 21:34), dim = c(4, 3, 2), order = "F")
#' place(a, oldvalue = c(1, 200, 3), newvalue = c(100, 2, 300))
#'
#' a <- zeros(dim = c(5, 4))
#' place(a, i = 2:4, j = 2:3, oldvalue = 0, newvalue = 2)
#'
#' @export
place <- function(a, ..., oldvalue, newvalue) {
  as <- slice(a, ...)
  old_index <- which(oldvalue %in% as)
  if (is.integer(old_index) && (length(old_index) == 0L)) # no match
    return(a)
  if (length(newvalue) > 1L) {
    stopifnot("newvalue must have the same length as oldvalue." = length(newvalue) == length(oldvalue))
    newvalue <- newvalue[old_index]
  }
  oldvalue <- oldvalue[old_index]
  aindex <- which(as %in% oldvalue)
  if (length(newvalue) == 1L) {
    as[aindex] <- newvalue
  } else {
    vindex <- match(as, oldvalue)
    vindex <- vindex[!is.na(vindex)]
    as[aindex] <- newvalue[vindex]
  }
  slice(a, ...) <- as
  a
}

#' @title Array searching
#' @description Manipulate array elements depending on conditions.
#'
#' @param a An array.
#' @param condition Logical expression indicating elements to search for.
#' @param true Replacement value or function if conditions are met.
#' @param false Replacement value or function if conditions aren't met.
#'
#' @details This function corresponds to \code{where()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.where.html}{see}). \cr
#'
#' The result is an array with the same shape as \code{a}, where the elements in \code{a} that satisfy or do not satisfy the given condition are processed in a specific way and placed at the same position as in \code{a} in the result.
#' Elements that satisfy the condition are processed according to the specification for \code{true}, elements that do not satisfy the condition are processed according to the specification for \code{false}.
#' If \code{true} or \code{false} are \code{NULL} the original values from \code{a} remain. For both arguments concrete values as well as functions can be specified.
#'
#' @return An array of same shape as \code{a} with elements either from \code{true} if conditions are met or from \code{false} elsewhere.
#' @examples
#' a <- marray(1:24, dim = c(4, 3, 2))
#' where(a, (a >= 11) & (a < 20), true = 23, false = 67)
#' where(a, a < 11, true = function(x) {x * 100}, false = 0)
#' where(a, a < 11, true = \(x) x * 100, false = 0) # lamdba-like anonymous function syntax \(x) from R 4.1
#'
#' @export
where <- function(a, condition, true, false) {
  a <- .standardize_array(a)
  e <- substitute(condition)
  idx <- eval(e, parent.frame())
  if (!is.logical(idx))
    stop("'condition' must be logical.", call. = FALSE)
  idx <- idx & !is.na(idx)

  idxTRUE <- which(idx)
  idxFALSE <- which(!idx) # setdiff(seq_len(nsize(a)), idxTRUE)
  if (!is.null(true))
    if (!is.function(true))
      a[idxTRUE] <- true
    else
      if (length(idxTRUE)) a[idxTRUE] <- forceAndCall(1, true, a[idxTRUE]) #true(a[idxTRUE])
  if (!is.null(false))
    if (!is.function(false))
      a[idxFALSE] <- false
    else
      if (length(idxFALSE)) a[idxFALSE] <- forceAndCall(1, false, a[idxFALSE])# false(a[idxFALSE])
  a
}

# helper function
.identical <- function(x, y) if (identical(x, y)) x else FALSE

#' @title Bitwise array operation
#' @description Bitwise AND, OR, or XOR concatenation of position equal elements of the given arrays.
#'
#' @param ... Any number of arrays all with identical shapes.
#' @param op An operator indicating the type of bitwise operation.
#'   * \code{and}: Bitwise AND concatenation
#'   * \code{or}: Bitwise OR concatenation
#'   * \code{xor}: Bitwise XOR concatenation
#'
#' @return An array with the same shape as the given arrays with bitwise combined elements.
#'
#' @examples
#' a <- marray(c(0, 0, 0, 1, 1, 1, 0, 0, 1), dim = c(3, 3, 1))
#' b <- marray(c(0, 1, 0, 0, 1, 1, 0, 1, 0), dim = c(3, 3, 1))
#' c <- marray(c(0, 0, 1, 0, 1, 0, 0, 1, 1), dim = c(3, 3, 1))
#' bitwise(a, b, c, op = "and")
#' bitwise(a, b, c, op = "or")
#' bitwise(a, b, c, op = "xor")
#'
#' @export
bitwise <- function(..., op = c("and", "or", "xor")) {
  op <- match.arg(op)
  arys <- .dots(...)
  n <- length(arys)
  stopifnot("There must be at least two arrays for a bitwise combination." = n >= 2,
            "All array dimensions must be identical." = Reduce(.identical, lapply(arys, DIM)) != FALSE)
  d <- DIM(arys[[1]])
  arys <- lapply(arys, FUN = flatten)
  a <- arys[[1L]]
  switch(op,
    and = {
      for (i in seq.int(2L, n))
        a <- bitwAnd(a, arys[[i]])
    },
    or = {
      for (i in seq.int(2L, n))
        a <- bitwOr(a, arys[[i]])
    },
    xor = {
      for (i in seq.int(2L, n))
        a <- bitwXor(a, arys[[i]])
    }
  )
  marray(a, dim = d)
}

#' @title Array searching
#' @description Clip (limit) the values in an array.
#'
#' @param a An array.
#' @param ... Indexing instructions with or without letters in form of \code{name = value} pairs. The names of the arguments specify the axis and the values its positions.
#' @param a_min Minimum value.
#' @param a_max Maximum value.
#'
#' @details This function corresponds partially to \code{clip()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.clip.html}{see}).
#' In opposite to NumPy function a slice of \code{a} can be specified to which limitation of the values refers.
#'
#' @return The array \code{a}, but where values < \code{a_min} are replaced with \code{a_min}, and those > \code{a_max} with \code{a_max}.
#'
#' @examples
#' a <- marray(1:24, dim = c(4, 3, 2))
#' # Limit entire array
#' maclip(a, a_min = 3, a_max = 7)
#' # Limit only a region of the array
#' maclip(a, i = 1:2, j = 2:3, a_min = 3, a_max = 7)
#'
#' @export
maclip <- function(a, ..., a_min, a_max) {
  as <- slice(a, ...)
  if (a_min > a_max)
    slice(as) <- a_max
  else {
    as[as < a_min] <- a_min
    as[as > a_max] <- a_max
  }
  slice(a, ...) <- as
  a
}

#' @title Array searching
#' @description Get indices of maximum or minimum values.
#'
#' @param a An array.
#' @param axis The axis along which to search for values. By default (\code{NULL}), the index is into the flattened array.
#'
#' @details This function corresponds to \code{argmax()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.argmax.html}{see}).
#'
#' @return Array of indices into the array. It has the same shape as \code{a} with the dimension along axis removed.
#'
#' @examples
#' a <- marray(c(1:8, 8), dim = c(3, 3, 1))
#' argmax(a)
#' argmax(a, axis = 1)
#' argmax(a, axis = 2)
#' argmax(a, axis = 3)
#'
#' @export
argmax <- function(a, axis = NULL) {
  a <- .standardize_array(a)
  if (is.null(axis))
    return(which.max(flatten(a)))
  d <- DIM(a)
  axis <- .standardize_axis(axis, length(d))
  apply(a, MARGIN = seq_along(d)[-axis], which.max)
}

#' @rdname argmax
#' @details This function corresponds to \code{argmin()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.argmin.html}{see}).
#' @export
argmin <- function(a, axis = NULL) {
  a <- .standardize_array(a)
  if (is.null(axis))
    return(which.min(flatten(a)))
  d <- DIM(a)
  axis <- .standardize_axis(axis, length(d))
  apply(a, MARGIN = seq_along(d)[-axis], which.min)
}

#' @title Array searching
#' @description Find the indices of those elements of an array that satisfy conditions.
#'
#' @param a An array.
#' @param condition Logical expression indicating elements to keep.
#'
#' @details This function corresponds partially to \code{argwhere()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.argwhere.html}{see}).
#'
#' @return Indices of elements of \code{a} that meet \code{condition}, otherwise \code{NULL}.
#'
#' @examples
#' a <- marray(0:5, dim = c(2, 3))
#' argwhere(a, a > 1)
#'
#' @export
argwhere <- function(a, condition) {
  do.call(rbind, axesindices(a, extract(a, condition)))
}

#' @title Array searching
#' @description Return the maximum of an array or maximum along an axis.\cr
#'   Return the minimum of an array or minimum along an axis.
#'
#' @param a An array.
#' @param axis Axis or axes along which to operate. By default (\code{NULL}), flattened input is used.
#'
#' @details This function corresponds to \code{amax()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.amax.html}{see}).
#'
#' @return Maximum of \code{a}. If \code{axis} is \code{NULL}, the result is a scalar value. If \code{axis} is an int, the result is an array of dimension \code{\link{ndim}} - 1. If \code{axis} is a tuple, the result is an array of dimension \code{\link{ndim}} - \code{length(axis)}.
#'
#' @examples
#' a <- marray(c(0:3), dim = c(2, 2))
#' amax(a)
#' amax(a, axis = 1)
#' amax(a, axis = 2)
#'
#' amin(a)
#' amin(a, axis = 1)
#' amin(a, axis = 2)
#'
#' @export
amax <- function(a, axis = NULL) {
  a <- .standardize_array(a)
  d <- DIM(a)
  axis <- .standardize_axis(axis, length(d))
  along <- seq_along(d)[-axis]
  if ((is.null(axis)) || (length(along) == 0L))
    return(max(flatten(a)))
  else
    return(apply(a, along, max))
}

#' @rdname amax
#' @details This function corresponds to \code{amin()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.amin.html}{see}).
#' @return Minimum of \code{a}. If \code{axis} is \code{NULL}, the result is a scalar value. If \code{axis} is an int, the result is an array of dimension \code{\link{ndim}} - 1. If \code{axis} is a tuple, the result is an array of dimension \code{\link{ndim}} - \code{length(axis)}.
#' @export
amin <- function(a, axis = NULL) {
  a <- .standardize_array(a)
  d <- DIM(a)
  axis <- .standardize_axis(axis, length(d))
  along <- seq_along(d)[-axis]
  if ((is.null(axis)) || (length(along) == 0L))
    return(min(flatten(a)))
  else
    return(apply(a, along, min))
}

#' @title Array counting
#' @description Counts the number of non-zero values in an array.
#'
#' @param a An array.
#' @param axis The axis or tuple of axes along which to count non-zeros. By default (\code{NULL}), meaning that non-zeros will be counted along a flattened version of \code{a}.
#'
#' @details This function corresponds to \code{count_nonzero()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.count_nonzero.html}{see}).\cr
#' If axis is given, the result array has the same shape as \code{a} with the dimension along axis removed.
#'
#' @return The number of non-zeros along a given axis. Otherwise, the total number of non-zeros in the array.
#'
#' @examples
#' a <- marray(c(0, 1, 7, 0, 3, 0, 2, 19), dim = c(2, 4, 1))
#' count_nonzero(a)
#' count_nonzero(a, axis = 1)
#' count_nonzero(a, axis = 2)
#'
#' @export
count_nonzero <- function(a, axis = NULL) {
  a <- .standardize_array(a)
  if (is.null(axis))
    return(sum(a != 0))
  d <- seq_along(DIM(a))
  axis <- .standardize_axis(axis, length(d))
  apply(a, MARGIN = d[-axis], function(x) sum(x != 0))
}

#' @title Array counting
#' @description Return indices that are non-zero in a flatten array.
#'
#' @param a An array.
#'
#' @details This function corresponds to \code{flatnonzero()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.flatnonzero.html}{see}).\cr
#'
#' @return Vector containing the indices of non-zero elements within \code{a}.
#'
#' @examples
#' a <- marray(seq(-2, 2))
#' flatnonzero(a)
#'
#' # Use the indices of the non-zero elements as an index vector to extract these elements
#' flatten(a)[flatnonzero(a)]
#'
#' @export
flatnonzero <- function(a) {
  a <- .standardize_array(a)
  which(flatten(a) != 0)
}

#' @title Array manipulation
#' @description Trim the leading and/or trailing zeros from a 1D array.
#'
#' @param filt 1D input array.
#' @param trim A string with ‘f’ representing trim from front and ‘b’ to trim from back. Default is ‘fb’, trim zeros from both front and back of the array.
#'
#' @details This function corresponds to \code{trim_zeros()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.trim_zeros.html}{see}).\cr
#'
#' @return A 1D array of trimming the input.
#'
#' @examples
#' a <- marray(c(0, 0, 0, 1, 2, 3, 0, 2, 1, 0))
#' trim_zeros(a)
#' trim_zeros(a, 'b')
#'
#' @export
trim_zeros <- function(filt, trim = c("fb", "f", "b")) {
  filt <- flatten(filt)
  trim <- match.arg(trim)
  switch (trim,
    fb = {
      filt <- filt[min(which(filt != 0)) : max(which(filt != 0))]
    },
    f = {
      filt <- filt[min(which(filt != 0)) : length(filt)]
    },
    b = {
      filt <- filt[1L : max(which(filt != 0))]
    }
  )
  return(filt)
}

#' @title Array math
#' @description Calculate the n-th discrete difference along the given axis.
#'
#' @param a An array.
#' @param n The number of times values are differenced. If zero, the input is returned as-is.
#' @param axis The axis along which the difference is taken, default is the last axis.
#' @param order The order in which elements of data should be read during operation.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds to \code{diff()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.diff.html}{see}).
#'
#' @return An array with the n-th differences. The shape of the output is the same as \code{a} except along \code{axis} where the dimension is smaller by \code{n}.
#'
#' @examples
#' a <- marray(c(3, 4, 8, 2, 6, 5), dim = c(2, 3))
#' madiff(a, axis = 1)
#' madiff(a, axis = 2)
#'
#' a <- marray(c(11, 2, 4, 10, 13, 5, 1, 8, 3, 9, 10, 11, 2, 19, 4, 4, 10, 11, 23, 67, 69, 47, 21, 23), dim = c(4, 3, 2))
#' madiff(a)
#' madiff(a, axis = 1)
#' madiff(a, axis = 2)
#'
#' @export
madiff <- function(a, n = 1, axis = -1, order = c("C", "F")) {
  a <- .standardize_array(a)
  order <- match.arg(order)
  d <- DIM(a)
  axis <- .standardize_axis(axis, length(d))
  stopifnot("The length of axis must be less or equal the number of axis of a." = length(axis) <= length(d))
  if (any(n >= d[axis]))
    n <- min(d[axis]) - 1L
  if (length(d) == 1L)
    return(marray(diff(a, differences = n)))
  else {
    axes <- as.list(rep(1, length(d)))
    axes[[axis]] <- NA
    iter <- seq(length(d))[-axis]
    d[axis] <- d[axis] - n
    out <- empty(dim = d)
    seq_order <- if (identical(order, "C")) rev(iter) else iter
    for (j in seq_len(prod(d[iter]))) {
      out <- copyto(out, dst_axis_index = axes, diff(slice(a, axes, drop = TRUE), differences = n))
      for (i in seq_order) {
        axes[[i]] <- axes[[i]] + 1
        if (axes[[i]] <= d[i])
          break
        else
          axes[[i]] <- 1
      }
    }
    return(out)
  }
}
