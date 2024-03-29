% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/delete.r
\name{delete}
\alias{delete}
\alias{erase}
\title{Array deletion}
\usage{
delete(a, ..., drop = FALSE)

erase(a, axis = 1L, keep = c("first", "last"), order = c("C", "F"))
}
\arguments{
\item{a}{An array.}

\item{...}{Indexing instructions with or without letters in form of \code{name = value} pairs. The names of the arguments specify the axis and the values its positions.}

\item{drop}{For matrices and arrays. If \code{TRUE} the result is coerced to the lowest possible dimension. This only works for extracting elements, not for the replacement. See \code{\link[base]{drop}} for further details.}

\item{axis}{The axis or axes to delete from \code{a}.}

\item{keep}{The direction data of \code{a} are read for the newly created array. \code{first} denotes a reading of the first n-elements and \code{last} of last n elements.}

\item{order}{The order in which elements of \code{x} should be read during recreation after deleting \code{axis}.
By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.}
}
\value{
The array \code{a} with deleted positions per axis.

The array \code{a} with deleted axes.
}
\description{
Delete parts of an array.

Delete whole axis from an array.
}
\details{
This function corresponds partially to \code{delete()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.delete.html}{see}).

The number of elements is tailored for the reshaped array. The argument \code{keep} determines the reading direction for tailoring.
}
\examples{
# original array
a <- marray(1:24, dim = c(4, 3, 2), order = "F")
# delete nothing
delete(a)
# delete positions 1 and 2 of the first axis
delete(a, i = 1:2)
# delete second row and third column
delete(a, i = 2, j = 3)

# original array
a <- marray(1:24, dim = c(4, 3, 2), order = "F")
# delete the first dimension with reading first n elements
erase(a)
# delete the first dimension with reading last n elements
erase(a, keep = "last")
# delete the axes one and two with reading first n elements
erase(a, axis = c(1L, 2L))

}
