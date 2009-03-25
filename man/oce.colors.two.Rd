\name{oce.colors}
\alias{oce.colors}
\title{Two-hue colormap}
\description{Two-hue colormap}
\usage{oce.colors(n, low=2/3, high=0, smax=1, alpha = 1)}
\arguments{
\item{n}{the number of colors (\eqn{\ge 1}{>=1}) to be in the palette.}
\item{low}{the hue for the low end of the palette, a number in [0,1], as
  an argument to \code{\link{hsv}}.  The default is blue.}
\item{high}{as for \code{low}, except for high end of the palette.}
\item{smax}{the saturation at the ends of the palette, a number in [0,1].
  The  default is full saturation.}
\item{alpha}{the alpha transparency, a number in [0,1]; see argument
  \code{alpha} in \code{\link{hsv}}.}
}

\details{The default is a palette running from fully saturated blue,
  through white, to fully saturated red.  This has a simple (and perhaps
  intuitive) physical interpreations, with colour going from cold to
  warm, as values increase from minimum to maximum.

  This function is patterned directly on \code{\link{cm.colors}}, with
  the differences being only that the endpoint hues and saturation may
  be controlled here.  The default palette from \code{oce.colors} is
  more intense than the \code{\link{cm.colors}}, because the latter has
  endpoint saturation of 0.5.
}

\examples{
library(oce)
x <- array(1:1000, dim=c(1,1000))
par(mfrow=c(1,2))
image(x, col=oce.colors(100))
image(x, col=cm.colors(100))
}
\author{Dan Kelley}
\keyword{misc}
