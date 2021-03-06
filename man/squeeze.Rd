% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reshape.r
\name{squeeze}
\alias{squeeze}
\title{Array compressing}
\usage{
squeeze(a, axis = NULL, order = c("C", "F"))
}
\arguments{
\item{a}{An array.}

\item{axis}{The dimensions which should be removed. If \code{NULL} (default), all dimensions of length one are removed.}

\item{order}{The order in which elements of data should be read during rearrangement after removing of corresponding dimensions.
By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.}
}
\value{
The array \code{a} usually without dimensions of length one.
}
\description{
Compress the shape of an array by removing singleton dimensions.
}
\details{
This function corresponds to \code{squeeze()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.squeeze.html}{see}).
  The base R function \code{\link{drop}} does the same as this function. In opposite to \code{drop} this function
  allows reordering the elements of the newly created array as well as specifying only certain axes.
}
\seealso{
\code{\link{drop}}, \code{\link{expand_dims}}.
}
