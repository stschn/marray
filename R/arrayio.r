#' @title Array Serialization
#' @description Save and load arrays.
#'
#' @param ... Any number of arrays. The arrays can be packed into a \code{\link{list}} to make code more pipe-friendly.
#' @param file The names of the files or a folder the arrays are stored into or restored from.
#' @param fileext The extension of the stored array files, default is \code{.rds}.
#' @details If \code{file} is a folder, the names of the arguments are used as physical file names to store the arrays.
#'   For restoring, if \code{file} is a folder all files with the extension \code{fileext} are restored as arrays and packed into a list.
#'
#' @return \code{NULL} invisibly.
#'
#' @seealso \code{\link{saveRDS}}.
#'
#' @examples
#' a <- marray(1:12, dim = c(4, 3))
#' b <- marray(1:12, dim = c(4, 3, 1))
#' c <- marray(1:12, dim = c(4, 3, 1, 1))
#' array_save(a, b, c, file = getwd())
#'
#' @export
array_save <- function(..., file = "", fileext = ".rds") {
  arys <- .dots(...)
  arys_name <- if (!(file_test("-d", file)))
    if (identical(length(arys), length(file)))
      file
    else stop(sprintf("file must be of the same length as the number of arrays passed (%d).", length(arys)))
    else {
      fn <- substitute(...())
      filename <- if (is.call(fn[[1L]])) sapply(fn[[1L]][-1L], deparse) else sapply(fn, deparse)
      folder <- normalizePath(file)
      if (!substr(folder, nchar(folder) - 1L, nchar(folder)) == "\\")
        folder <- paste0(folder, "\\")
      filename <- unname(sapply(filename, function(f) paste0(folder, f, fileext)))
      filename
    }
  sapply(seq_along(arys), function(i) saveRDS(arys[[i]], arys_name[[i]]))
  return(invisible(NULL))
}

#' @rdname array_save
#' @return An array or a list of restored arrays.
#' @seealso \code{\link{readRDS}}.
#' @export
array_load <- function(file = "", fileext = ".rds") {
  if (file_test("-d", file)) {
    arys <- lapply(list.files(file, pattern = paste0("\\", fileext), full.names = TRUE), readRDS)
    if (length(arys) == 1L) arys <- arys[[1L]]
  } else
    arys <- readRDS(file)
  arys
}
