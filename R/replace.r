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
#' @details This function corresponds to \code{where()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.where.html}{see}).
#'   If \code{true} or \code{false} are \code{NULL} the original values remain. For both arguments also functions can be specified.
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
  idxFALSE <- setdiff(seq_len(nsize(a)), idxTRUE)
  if (!is.null(true))
    if (!is.function(true))
      a[idxTRUE] <- true
    else
      if (length(idxTRUE)) a[idxTRUE] <- true(a[idxTRUE])
  if (!is.null(false))
    if (!is.function(false))
      a[idxFALSE] <- false
    else
      if (length(idxFALSE)) a[idxFALSE] <- false(a[idxFALSE])
  a
}

#' @title Array searching
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
