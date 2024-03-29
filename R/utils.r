# Internal helper functions

# A simple wrapper for transformation of an R object into an array
.standardize_array <- function(a) {
  if (!is(a, "array"))
    a <- as.array(a)
  a
}

# Simplifies arguments (...) to a list
# Packaging arguments externally into a list makes function calls pipe-friendly.
.dots <- function(..., simplify = TRUE) {
  args <- list(...)
  if (simplify)
    if (any(sapply(args, is.list))) args <- unlist(args, recursive = FALSE)
  args
}

# Retrieve letters for indexing
.axis_symbol <- function(axis) { letters[abs(axis) + 8L] }

# Indexing of an array
# Indexing can be done with or without symbols i, j, k etc. All indices are proofed and adopted, if necessary.
.aindex <- function(dim, ...) {
  ds <- lapply(dim, seq_len)
  args <- .dots(...)
  if (length(args) > length(dim))
    stop(sprintf("number of arguments for indexing (%d) is greater than the number of dimensions (%d).", length(args), length(dim)), call. = FALSE)
  axis <- match(names(args), letters) - 8L
  if (!((is.integer(axis) && !length(axis)) || any(is.na(axis)))) {
    if (any(axis < 1L))
      stop(sprintf("symbols (%s) are not allowed as arguments for indexing.", paste(letters[axis[which(axis < 1L)] + 8L], collapse = ", ")), call. = FALSE)
    if (max(axis) > length(dim))
      stop(sprintf("number of arguments for indexing (%d) is greater than the number of dimensions (%d).", max(axis), length(dim)), call. = FALSE)
    ds[axis] <- args
  } else {
    args <- lapply(seq_along(args), function(i) if (any(is.null(args[[i]])) || any(is.na(args[[i]])) || any(args[[i]] == 0)) ds[[i]] else args[[i]])
    ds[seq_along(args)] <- args
  }
  ds <- lapply(seq_along(ds), function(i) {
    x <- ds[[i]]
    x[(x > dim[i])] <- dim[i]
    if (any(x == 0)) x <- seq(dim[i])
    x
  })
  ds
}

# Axis interpretation as follows:
# 0 is set to 1
# Positive numbers as are
# Negative Numbers are axes read from back to front
.standardize_axis <- function(axis, ndim = 1L) {
  ndim <- as.integer(abs(ndim[1L]))
  axis[which(axis == 0L)] <- 1L
  neg <- which(axis < 0L)
  axis[neg][which(axis[neg] < -ndim)] <- -ndim
  axis[neg] <- ndim + axis[neg] + 1L #ndim - abs(axis[neg]) + 1L
  axis[which(axis > ndim)] <- ndim
  axis
}

# During reshaping of an array, one axis of the new shape can be -1.
# In this case, the value is inferred from the size of the array and remaining dimensions and automatically computed.
.standardize_shape <- function(adim, newdim = NULL) {
  if (is.null(newdim)) return(newdim)
  mapidx <- unique(which(newdim %in% -1))
  if (!length(mapidx)) return(newdim) # empty integer indicates that no -1 is inside newdim
  stopifnot("-1 can only be used for one axis" = length(mapidx) == 1)
  newdim[mapidx] <- prod(adim) / as.integer(prod(newdim[-mapidx]))
  newdim
}
