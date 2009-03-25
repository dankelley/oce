\name{oce.colors.jet}

\alias{oce.colors.jet}

\title{Spectal colormap, similar to matlab Jet}

\description{Spectal colormap, similar to matlab Jet}

\usage{oce.colors.jet(n)}

\arguments{
\item{n}{the number of colors (\eqn{\ge 1}{>=1}) to be in the palette.}
}

\details{The default, patterned on the Matlab jet palette, runs from
  blue at low values, to a thin turquoise band, to a wide green band, to
  a thin yellow band, to a red band.}

\examples{
library(oce)
x <- array(1:1000, dim=c(1,1000))
par(mfrow=c(1,3))
image(x, col=oce.colors.two(100), main="oce.colors.two")
image(x, col=oce.colors.jet(100), main="oce.colors.jet")
image(x, col=oce.colors.gebco(100), main="oce.colors.gebco")
}

\author{Dan Kelley}

\keyword{misc}
