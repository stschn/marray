#' @title Array window
#' @description Return the Bartlett window.
#'
#' @param M Number of points in the output window. If zero or less, an empty array is returned.
#'
#' @details This function corresponds to \code{bartlett()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.bartlett.html}{see}).\cr
#' The Bartlett window is defined as
#'
#' \deqn{w(n) = \frac{2}{M-1} \left(\frac{M-1}{2} - \left|n - \frac{M-1}{2}\right|\right)}
#'
#' @return The triangular window, with the maximum value normalized to one (the value one appears only if the number of samples is odd), with the first and last samples equal to zero.
#'
#' @references Bartlett M. S. (1950): Periodogram Analysis and Continuous Spectra, In: Biometrika 37(1-2), pp. 1-16.
#'
#' @seealso \code{\link{blackman}}, \code{\link{hamming}}, \code{\link{hanning}}, \code{\link{kaiser}}
#'
#' @export
bartlett <- function(M) {
  if (M < 1) return(full(1L))
  if (M == 1) return(ones(1L))
  a <- seq(1L - M, M, 2L)
  return(where(a, a <= 0, true = \(x) { 1 + x / (M - 1) }, false = \(x) { 1 - x / (M - 1) }))
}

#' @title Array window
#' @description Return the Blackman window.
#'
#' @param M Number of points in the output window. If zero or less, an empty array is returned.
#'
#' @details This function corresponds to \code{blackman()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.blackman.html}{see}).\cr
#' The Blackman window is defined as
#'
#' \deqn{w(n) = 0.42 - 0.5 \cos(2\pi n/M) + 0.08 \cos(4\pi n/M)}
#'
#' @return The window, with the maximum value normalized to one (the value one appears only if the number of samples is odd).
#'
#' @references Blackman, R.B. and Tukey, J.W. (1958): The measurement of power spectra, Dover Publications, New York.\cr
#' Oppenheim, A.V., R.W. Schafer (1999): Discrete-Time Signal Processing. Upper Saddle River, NJ: Prentice-Hall. pp. 468-471.
#'
#' @seealso \code{\link{bartlett}}, \code{\link{hamming}}, \code{\link{hanning}}, \code{\link{kaiser}}
#'
#' @export
blackman <- function(M) {
  if (M < 1) return(full(1L))
  if (M == 1) return(ones(1L))
  a <- seq(1L - M, M, 2L)
  return(as.marray(0.42 + 0.5 * cos(pi * a / (M - 1)) + 0.08 * cos(2.0 * pi * a / (M - 1))))
}

#' @title Array window
#' @description Return the Hamming window.
#'
#' @param M Number of points in the output window. If zero or less, an empty array is returned.
#'
#' @details This function corresponds to \code{hamming()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.hamming.html}{see}).\cr
#' The Hamming window is defined as
#'
#' \deqn{w(n) = 0.54 - 0.46\cos\left(\frac{2\pi{n}}{M-1}\right) \qquad 0 \leq n \leq M-1}
#'
#' @return The window, with the maximum value normalized to one (the value one appears only if the number of samples is odd).
#'
#' @references Blackman, R.B. and Tukey, J.W. (1958): The measurement of power spectra, Dover Publications, New York.
#'
#' @seealso \code{\link{bartlett}}, \code{\link{blackman}}, \code{\link{hanning}}, \code{\link{kaiser}}
#'
#' @export
hamming <- function(M) {
  if (M < 1) return(full(1L))
  if (M == 1) return(ones(1L))
  a <- seq(1L - M, M, 2L)
  return(as.marray(0.54 + 0.46 * cos(pi * a / (M - 1))))
}

#' @title Array window
#' @description Return the Hanning window.
#'
#' @param M Number of points in the output window. If zero or less, an empty array is returned.
#'
#' @details This function corresponds to \code{hanning()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.hanning.html}{see}).\cr
#' The Hanning window is defined as
#'
#' \deqn{w(n) = 0.5 - 0.5\cos\left(\frac{2\pi{n}}{M-1}\right) \qquad 0 \leq n \leq M-1}
#'
#' @return The window, with the maximum value normalized to one (the value one appears only if the number of samples is odd).
#'
#' @references Blackman, R.B. and Tukey, J.W. (1958): The measurement of power spectra, Dover Publications, New York.
#'
#' @seealso \code{\link{bartlett}}, \code{\link{blackman}}, \code{\link{hamming}}, \code{\link{kaiser}}
#'
#' @export
hanning <- function(M) {
  if (M < 1) return(full(1L))
  if (M == 1) return(ones(1L))
  a <- seq(1L - M, M, 2L)
  return(as.marray(0.5 + 0.5 * cos(pi * a / (M - 1))))
}

#' @title Array window
#' @description Return the Kaiser window.
#'
#' @param M Number of points in the output window. If zero or less, an empty array is returned.
#' @param beta Shape parameter for window. The Kaiser can approximate many other windows by varying the beta parameter:
#' * \code{0}: Rectangular.
#' * \code{5}: Similar to a Hamming.
#' * \code{6}: Similar to a Hanning.
#' * \code{8.6}: Similar to a Blackman.
#' @md
#'
#' @details This function corresponds to \code{kaiser()} from NumPy (\href{https://numpy.org/doc/stable/reference/generated/numpy.kaiser.html}{see}).\cr
#' The Kaiser window is defined as
#'
#' \deqn{w(n) = I_0\left(\beta \sqrt{1-\frac{4n^2}{(M-1)^2}} \right)/I_0(\beta)} with \eqn{\quad -\frac{M-1}{2} \leq n \leq \frac{M-1}{2}}
#'
#' @return The window, with the maximum value normalized to one (the value one appears only if the number of samples is odd).
#'
#' @references Kaiser, J. F. (1966): Digital Filters. In: F.F. Kuo and J.F. Kaiser (Eds.), Systems analysis by digital computer, pp. 218-285. New York: John Wiley and Sons.
#'
#' @seealso \code{\link{bartlett}}, \code{\link{blackman}}, \code{\link{hamming}}, \code{\link{hanning}}
#'
#' @export
kaiser <- function(M, beta) {
  if (M < 1) return(full(1L))
  if (M == 1) return(ones(1L))
  a <- seq(0, M - 1)
  alpha <- (M - 1) / 2.0
  numerator <- as.vector(i0(beta * sqrt(1 - ((a - alpha) / alpha)^2.0)))
  denominator <- as.vector(i0(as.numeric(beta)))
  return(as.marray(numerator / denominator))
}

# Helper functions adopted from NumPy

.i0A = c(
  -4.41534164647933937950e-18,
  3.33079451882223809783e-17,
  -2.43127984654795469359e-16,
  1.71539128555513303061e-15,
  -1.16853328779934516808e-14,
  7.67618549860493561688e-14,
  -4.85644678311192946090e-13,
  2.95505266312963983461e-12,
  -1.72682629144155570723e-11,
  9.67580903537323691224e-11,
  -5.18979560163526290666e-10,
  2.65982372468238665035e-9,
  -1.30002500998624804212e-8,
  6.04699502254191894932e-8,
  -2.67079385394061173391e-7,
  1.11738753912010371815e-6,
  -4.41673835845875056359e-6,
  1.64484480707288970893e-5,
  -5.75419501008210370398e-5,
  1.88502885095841655729e-4,
  -5.76375574538582365885e-4,
  1.63947561694133579842e-3,
  -4.32430999505057594430e-3,
  1.05464603945949983183e-2,
  -2.37374148058994688156e-2,
  4.93052842396707084878e-2,
  -9.49010970480476444210e-2,
  1.71620901522208775349e-1,
  -3.04682672343198398683e-1,
  6.76795274409476084995e-1
)

.i0B = c(
  -7.23318048787475395456e-18,
  -4.83050448594418207126e-18,
  4.46562142029675999901e-17,
  3.46122286769746109310e-17,
  -2.82762398051658348494e-16,
  -3.42548561967721913462e-16,
  1.77256013305652638360e-15,
  3.81168066935262242075e-15,
  -9.55484669882830764870e-15,
  -4.15056934728722208663e-14,
  1.54008621752140982691e-14,
  3.85277838274214270114e-13,
  7.18012445138366623367e-13,
  -1.79417853150680611778e-12,
  -1.32158118404477131188e-11,
  -3.14991652796324136454e-11,
  1.18891471078464383424e-11,
  4.94060238822496958910e-10,
  3.39623202570838634515e-9,
  2.26666899049817806459e-8,
  2.04891858946906374183e-7,
  2.89137052083475648297e-6,
  6.88975834691682398426e-5,
  3.36911647825569408990e-3,
  8.04490411014108831608e-1
)

.chbevl <- function(x, vals) {
  b0 <- vals[1L]
  b1 <- 0.0
  for (i in seq(2, length(vals))) {
    b2 <- b1
    b1 <- b0
    b0 <- x * b1 - b2 + vals[i]
  }
  return(0.5 * (b0 - b2))
}

.i0_1 <- function(x) { return(exp(x) * .chbevl(x / 2.0 - 2, .i0A)) }
.i0_2 <- function(x) { return(exp(x) * .chbevl(32.0 / x - 2.0, .i0B) / sqrt(x)) }

# Modified Bessel function of the first kind, order 0.
i0 <- function(x) {
  x <- abs(as.numeric(x))
  return(where(x, x <= 8.0, true = .i0_1, false = .i0_2))
}
