# Internal helper functions

# Simplifies arguments (...) to a list
# Packaging arguments externally into a list makes function calls pipe-friendly
.dots <- function(..., simplify = TRUE) {
  args <- list(...)
  if (simplify)
    if (any(sapply(args, is.list))) args <- unlist(args, recursive = FALSE)
  args
}
