# Internal helper functions

# Simplifies arguments (...) to a list
# Packaging arguments externally into a list makes function calls pipe-friendly.
.dots <- function(..., simplify = TRUE) {
  args <- list(...)
  if (simplify)
    if (any(sapply(args, is.list))) args <- unlist(args, recursive = FALSE)
  args
}

# Indexing of an array
# Indexing can be done with or without symbols i, j, k etc. All indices are proofed and adopted, if necessary.
.aindex <- function(dim, ...) {
  ds <- lapply(dim, seq_len)
  ds_min <- lapply(ds, min)
  ds_max <- lapply(ds, max)
  args <- .dots(...)
  if (length(args) > length(ds))
    stop(sprintf("number of arguments for indexing (%d) is greater than the number of dimensions (%d).", length(args), length(ds)), call. = FALSE)
  which <- match(names(args), letters) - 8L
  if (!((is.integer(which) && !length(which)) || any(is.na(which)))) {
    if (max(which) > length(ds))
      stop(sprintf("number of arguments for indexing (%d) is greater than the number of dimensions (%d).", max(which), length(ds)), call. = FALSE)
    ds[which] <- args
  } else {
    args <- lapply(seq_along(args), function(i) if (any(is.null(args[[i]])) || any(is.na(args[[i]])) || any(args[[i]] <= 0)) ds[[i]] else args[[i]])
    ds[seq_along(args)] <- args
  }
  ds <- lapply(seq_along(ds), function(i) {
    x <- ds[[i]]
    x[x < ds_min[[i]]] <- ds_min[[i]]
    x[x > ds_max[[i]]] <- ds_max[[i]]
    unique(x)
  })
  ds
}
