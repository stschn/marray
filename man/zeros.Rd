% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create.r
\name{zeros}
\alias{zeros}
\alias{zeros_like}
\title{Array creation}
\usage{
zeros(dim = NULL, dimnames = NULL)

zeros_like(a)
}
\arguments{
\item{dim}{Shape of the new array.}

\item{dimnames}{Either \code{NULL} or the names of the dimensions. This must be a list with one component for each dimension, either \code{NULL} or a character vector of the length given by \code{dim} for that dimension.}

\item{a}{An array whose shape is taken for the result array.}
}
\value{
An array of zeros with the given shape.

An array of zeros with the the same shape as \code{a}.
}
\description{
Create array filled with zeros.
}
\details{
This function corresponds to \code{zeros()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.zeros.html}{see}).

This function corresponds to \code{zeros_like()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.zeros_like.html#numpy.zeros_like}{see}).
}
