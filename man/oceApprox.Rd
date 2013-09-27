\name{oceApprox}
\alias{oceApprox}
\title{Interpolate with Reiniger-Ross algorithm}

\description{Interpolate with weighted-parabola method developed by Reiniger
    and Ross (1968).  (A modified version of this used in the NODC world ocean
    atlas, as explained on pages 48-50 of Johnson et al. 2011.)} 

\usage{oceApprox(x, y, xout, method=c("reiniger-ross"))}

\arguments{
  \item{x}{the independent variable (z or p, usually).}
  \item{y}{the dependent variable.}
  \item{xout}{the values of the independent variable at which
    interpolation is to be done.}
  \item{method}{method to use.  See \dQuote{Details}.}
}

\details{Only one \code{method} is provided at present.  This is the
  weighted-parabola algorithm of Reiniger and Ross (1968).  For
  procedure is as follows.  First, the interpolant for any \code{xout}
  value that is outside the range of \code{x} is set to NA.  Next,
  linear interpolation is used for any \code{xout} value that has only
  one smaller neighboring \code{x} value, or one larger neighboring
  value.  For all other values of \code{xout}, the 4 neighboring points
  \code{x} are sought, two smaller and two larger.  Then two parabolas
  are determined, one from the two smaller points plus the nearest
  larger point, and the other from the nearest smaller point and the two
  larger points.  A weighted sum of these two parabolas provides the
  interpolated value.

  Note that, in the notation of Reiniger and Ross (1968), this algorithm
  uses \code{m}=2 and \code{n}=1.  (A future version of this routine
  might provide the ability to modify these values.)
}

\value{A vector of interplated values, corresponding to the \code{xout}
  values and equal in number.}

\examples{
library(oce)
data(RRprofile)
zz <- seq(0,2000,5)
plot(RRprofile$temperature, RRprofile$depth, ylim=c(500,0), xlim=c(2,11))
TT <- oceApprox(RRprofile$depth, RRprofile$temperature, zz)
lines(TT, zz)
}

\references{R.F. Reiniger and C.K. Ross, 1968.  A method of
interpolation with application to oceanographic data.  \emph{Deep Sea
Research}, \bold{15}, 185-193.

Daphne R. Johnson, Tim P. Boyer, Hernan E. Garcia, Ricardo A. Locarnini, Olga
K. Baranova, and Melissa M. Zweng, 2011. World Ocean Database 2009
Documentation.  NODC Internal report 20.  Ocean Climate Laboratory, National
Oceanographic Data Center.  Silver Spring, Maryland.
}

\author{Dan Kelley}

\keyword{misc}
