% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/window.r
\name{blackman}
\alias{blackman}
\title{Array window}
\usage{
blackman(M)
}
\arguments{
\item{M}{Number of points in the output window. If zero or less, an empty array is returned.}
}
\value{
The window, with the maximum value normalized to one (the value one appears only if the number of samples is odd).
}
\description{
Return the Blackman window.
}
\details{
This function corresponds to \code{blackman()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.blackman.html}{see}).\cr
The Blackman window is defined as

\deqn{w(n) = 0.42 - 0.5 \cos(2\pi n/M) + 0.08 \cos(4\pi n/M)}
}
\references{
Blackman, R.B. and Tukey, J.W. (1958): The measurement of power spectra, Dover Publications, New York.\cr
Oppenheim, A.V., R.W. Schafer (1999): Discrete-Time Signal Processing. Upper Saddle River, NJ: Prentice-Hall. pp. 468-471.
}
\seealso{
\code{\link{bartlett}}, \code{\link{hamming}}, \code{\link{hanning}}, \code{\link{kaiser}}
}
