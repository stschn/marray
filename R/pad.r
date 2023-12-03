#' @title Array padding
#' @description Pad an array.
#'
#' @param a An array.
#' @param pad_width Number of values padded to the edges of each axis. ((before_1, after_1), ... (before_N, after_N)) unique pad widths for each axis. One integerish value is a shortcut for before = after for all axes.
#' @param mode The technique to be used for padding an array.
#'   * \code{constant}: Pads with a constant value.
#'   * \code{edge}: Pads with the edge values of array.
#'   * \code{linear_ramp}: Pads with the linear ramp between end_value and the array edge value.
#'   * \code{maximum}: Pads with the maximum value of all or part of the vector along each axis.
#'   * \code{mean}: Pads with the mean value of all or part of the vector along each axis.
#'   * \code{median}: Pads with the median value of all or part of the vector along each axis.
#'   * \code{minimum}: Pads with the minimum value of all or part of the vector along each axis.
#'   * \code{reflect}: Pads with the reflection of the vector mirrored on the first and last values of the vector along each axis.
#'   * \code{symmetric}: Pads with the reflection of the vector mirrored along the edge of the array.
#'   * \code{wrap}: Pads with the wrap of the vector along the axis. The first values are used to pad the end and the end values are used to pad the beginning.
#'   * \code{empty}: Pads with undefined (\code{NA}) values.
#' @md
#' @param ... Optional arguments corresponding to \code{mode}.
#'
#' @details This function corresponds to \code{pad()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.pad.html}{see}).
#'
#' @return Padded array of rank equal to \code{a} with shape increased according to \code{pad_width}.
#'
#' @examples
#' a <- marray(1:5)
#' pad(a, pad_width = list(c(2, 3)), constant_values = list(c(4, 6)))
#'
#' @export
pad <- function(a, pad_width, mode = c("constant", "edge", "linear_ramp", "maximum", "mean", "median", "minimum", "reflect", "symmetric", "wrap", "empty"), ...) {

  .as_pairs <- function(x = 1, ndim, pairs = c(0, 0)) {
    x <- as.list(x)
    if ((length(x) == 1L) & (length(x[[1L]]) == 1L)) {
      x <- lapply(seq_len(ndim), function(i) rep(x[[1L]], 2L))
    } else {
      while (length(x) < ndim) { x <- append(x, list(pairs)) }
      x <- lapply(seq_along(x), function(i) {
        if (length(x[[i]]) < 2L)
          rep(x[[i]], 2)
        else
          x[[i]][1L:2L]
      })
    }
    x
  }

  .mask <- 1e9

  a <- marray:::.standardize_array(a)
  mode <- match.arg(mode)
  args <- list(...)
  allowed_args <- setNames(c("constant_values"), c("constant"))
  d <- DIM(a)
  pad_width <- .as_pairs(pad_width, ndim(a))

  stopifnot("length of `pad_width` must be equal to the number of axes of `a`" = (length(pad_width) == ndim(a)),
            "each element of `pad_width` must be a pair (before, after)" = all(sapply(pad_width, length) == 2L))

  # Create array with final shape and original values
  newdim <- d + mapply(sum, pad_width)
  out <- empty(newdim)
  slice(out, lapply(seq_along(d), function(i) { seq(from = pad_width[[i]][1L] + 1, to = newdim[i] - pad_width[[i]][2L]) })) <- a

  argnames <- as.list(match.call()[-1])
  switch (mode,
    constant = {
      if (!allowed_args["constant"] %in% names(argnames))
        args[[allowed_args["constant"]]] <- 0
      values <- args[[allowed_args["constant"]]]
      values <- .as_pairs(values, ndim(a), pairs = c(NA, NA))
      for (i in seq_along(d)) {
        indices <- as.list(rep(NA, ndim(a)))
        # before the edge of axis i
        if (pad_width[[i]][1L] != 0) {
          indices[[i]] <- seq_len(pad_width[[i]][1L])
          slice(out, indices) <- values[[i]][1L]
        }
        # after the edge of axis i
        if (pad_width[[i]][2L] != 0) {
          indices[[i]] <- seq(from = newdim[i] - pad_width[[i]][2L] + 1L, to = newdim[i])
          slice(out, indices) <- values[[i]][2L]
        }
      }
    },
    edge = {
      for (i in seq_along(d)) {
        indices <- as.list(rep(NA, ndim(a)))
        # before the edge of axis i
        if (pad_width[[i]][1L] != 0) {
          out_indices <- indices
          out_indices[[i]] <- seq_len(pad_width[[i]][1L])
          indices[[i]] <- rep(pad_width[[i]][1L] + 1L, length(out_indices[[i]]))
          slice(out, out_indices) <- slice(out, indices)
        }
        indices <- as.list(rep(NA, ndim(a)))
        # after the edge of axis i
        if (pad_width[[i]][2L] != 0) {
          out_indices <- indices
          out_indices[[i]] <- seq(from = newdim[i] - pad_width[[i]][2L] + 1L, to = newdim[i])
          indices[[i]] <- rep(newdim[i] - pad_width[[i]][2L], length(out_indices[[i]]))
          slice(out, out_indices) <- slice(out, indices)
        }
      }
    },
    linear_ramp = {
      # pass
    },
    maximum = {
      out <- place(out, oldvalue = NA, newvalue = min(a) - .mask)
      for (i in seq_along(d)) {
        indices <- as.list(rep(NA, ndim(a)))
        # before the edge of axis i
        if (pad_width[[i]][1L] != 0) {
          indices[[i]] <- seq_len(pad_width[[i]][1L])
          slice(out, indices) <- apply_over_axes(out, axes = i, FUN = max, na.rm = TRUE)
        }
        # after the edge of axis i
        if (pad_width[[i]][2L] != 0) {
          indices[[i]] <- seq(from = newdim[i] - pad_width[[i]][2L] + 1L, to = newdim[i])
          slice(out, indices) <- apply_over_axes(out, axes = i, FUN = max, na.rm = TRUE)
        }
      }
    },
    mean = {
      # pass
    },
    median = {
      # pass
    },
    minimum = {
      out <- place(out, oldvalue = NA, newvalue = max(a) + .mask)
      for (i in seq_along(d)) {
        indices <- as.list(rep(NA, ndim(a)))
        # before the edge of axis i
        if (pad_width[[i]][1L] != 0) {
          indices[[i]] <- seq_len(pad_width[[i]][1L])
          slice(out, indices) <- apply_over_axes(out, axes = i, FUN = min, na.rm = TRUE)
        }
        # after the edge of axis i
        if (pad_width[[i]][2L] != 0) {
          indices[[i]] <- seq(from = newdim[i] - pad_width[[i]][2L] + 1L, to = newdim[i])
          slice(out, indices) <- apply_over_axes(out, axes = i, FUN = min, na.rm = TRUE)
        }
      }
    },
    reflect = {
      # pass
    },
    symmetric = {
      # pass
    },
    wrap = {
      # pass
    },
    empty = {
      # pass
    }
  )
  out
}
