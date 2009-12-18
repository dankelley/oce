\name{oce.approx}
\alias{oce.approx}
\title{Interpolate with Reiniger-Ross algorithm}
\description{Interpolate with Reiniger-Ross algorithm.}
\usage{oce.approx(x, y, xout, n=1, m=1)}

\arguments{
  \item{x}{the independent variable (z or p, usually).}
  \item{y}{the dependent variable.}
  \item{xout}{the values of the independent variable at which
    interpolation is to be done.}
  \item{n}{supplied value ignored (see \dQuote{Details}).}
  \item{m}{supplied value ignored (see \dQuote{Details}).}
}

\details{The algorithm is based on equation-sets 2, 3, and 3' of
  Reiniger and Ross (1968).  For \code{xout} values that have two or
  more \code{x} neighbors with smaller values, and two or more with
  larger values, equation-sets 2 and 3 are used.  

  \strong{BUG:} For other points, this returns NA or NaN.

  The values of \code{n} and \code{m} (exponents of terms in the
  equations) are set to 1 and 2 in the code, regardless of values
  supplied to this routine.  This is in accord with the recommendation
  of Reiniger and Ross (1968).}

\value{A vector of interplated values, corresponding to the \code{xout}
  values and equal in number.}

\examples{
library(oce)
data(RRprofile)
zz <- seq(0,2000,5)
plot(RRprofile$temperature, RRprofile$depth, ylim=c(500,0), xlim=c(2,11))
TT <- oce.approx(RRprofile$depth, RRprofile$temperature, zz)
lines(TT, zz)
}

\references{R.F. Reiniger and C.K. Ross, 1968.  A method of
interpolation with application to oceanographic data.  \emph{Deep Sea
Research}, \bold{15}, 185-193.}

\author{Dan Kelley}

\keyword{misc}
