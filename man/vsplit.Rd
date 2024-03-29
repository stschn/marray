% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split.r
\name{vsplit}
\alias{vsplit}
\title{Array splitting}
\usage{
vsplit(a, indices_or_sections)
}
\arguments{
\item{a}{An array to be divided into sub-arrays.}

\item{indices_or_sections}{An integerish vector indicating the type of splitting.
A single integer, N, denotes that the array \code{a} will be divided into N arrays along \code{axis}.
A sorted vector of integers denotes the entries where along \code{axis} the array \code{a} is split.}
}
\value{
A list of sub-arrays.
}
\description{
Split an array into multiple sub-arrays vertically (row-wise).
}
\details{
This function corresponds to \code{vsplit()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.vsplit.html}{see}).
It's equivalent to \code{\link{array_split}} along the first axis.
}
