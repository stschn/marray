% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rearrange.r
\name{crop}
\alias{crop}
\title{Array cropping}
\usage{
crop(a, ..., fill_value = NA)
}
\arguments{
\item{a}{An array.}

\item{...}{Indexing instructions with or without letters in form of \code{name = value} pairs. The names of the arguments specify the axis and the values its positions.}

\item{fill_value}{Value to fill the array beyond the slice defined by \code{...}.}
}
\value{
An array with the same shape as \code{a} including an original slice and with \code{fill_value} beyond the slice.
}
\description{
Crop array by selecting a slice and filling the rest with given values.
}
\examples{
a <- marray(1:24, dim = c(4, 3, 2))
crop(a, i = 1, j = 2:3)

}
