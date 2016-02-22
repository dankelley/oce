\name{plotPolar}

\alias{plotPolar}

\title{Draw a Polar Plot}

\description{Draw a polar plot.}

\usage{plotPolar(r, theta, debug=getOption("oceDebug"), \dots)}

\arguments{
    \item{r}{radii of points to plot.}
    \item{theta}{angles of points to plot, in degrees.}
    \item{debug}{a flag that turns on debugging.  Set to 1 to get a
        moderate amount of debugging information, or to 2 to get more.} 
    \item{\dots}{optional arguments passed to the lower-level plotting
        functions.}
}

\details{Creates a crude polar plot.}

\examples{
library(oce)
r <- rnorm(50, mean=2, sd=0.1)
theta <- runif(50, 0, 360)
plotPolar(r, theta)
}

\author{Dan Kelley}
\keyword{hplot}
