#' @title Array replacement
#' @description Replace array elements.
#'
#' @param a An array.
#' @param ... Indexing instructions with or without letters in form of \code{name = value} pairs. The names of the arguments specify the dimensions and the values its values.
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
#' a <- zeros(dim = c(5, 4))
#' mareplace(a, i = 2:4, j = 2:3, oldvalue = 0, newvalue = 2)
#'
#' @export
mareplace <- function(a, ..., oldvalue, newvalue) {
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
