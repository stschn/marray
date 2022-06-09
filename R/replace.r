#' @title Array replacement
#' @description Replace array elements.
#'
#' @param a An array.
#' @param oldvalue A vector of values to search for.
#' @param newvalue A vector of values to replace \code{oldvalue}.
#'   A single value is used as a replacement for all values in \code{oldvalue}. Otherwise, the length of \code{newvalue} must be equal to the length of \code{oldvalue}.
#'   In that case a value in \code{newvalue} at a certain position is used to replace the value in \code{oldvalue} at the same position.
#'
#' @return An array with replaced values.
#'
#' @examples
#' a <- marray(c(rep(1, 4), 2, rep(3, 5), 21:34), dim = c(4, 3, 2), order = "F")
#' mareplace(a, oldvalue = c(1, 200, 3), newvalue = c(100, 2, 300))
#'
#' @export
mareplace <- function(a, oldvalue, newvalue) {
  old_index <- which(oldvalue %in% a)
  if (is.integer(old_index) && (length(old_index) == 0L)) # no match
    return(a)
  if (length(newvalue) > 1L) {
    stopifnot("newvalue must have the same length as oldvalue." = length(newvalue) == length(oldvalue))
    newvalue <- newvalue[old_index]
  }
  oldvalue <- oldvalue[old_index]
  aindex <- which(a %in% oldvalue)
  if (length(newvalue) == 1L) {
    a[aindex] <- newvalue
  } else {
    vindex <- match(a, oldvalue)
    vindex <- vindex[!is.na(vindex)]
    a[aindex] <- newvalue[vindex]
  }
  a
}
