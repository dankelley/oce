\name{stickplot}

\alias{stickplot}

\title{Draw a stick-plot diagram}

\description{Draw a stick-plot diagram, with line segments indicating
  (x,y) values, arranged along a horizontal axis representing t.}

\usage{stickplot(t, x, y, \dots)}

\arguments{
  \item{t}{Time}
  \item{x}{horizontal component of line segment}
  \item{y}{vertical component of line segment}
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{At each time \code{t}, a line segment is drawn to represent the
  vector from the time axis to (\code{x}, \code{y}).}

\examples{
library(oce)
t <- seq.POSIXt(from=as.POSIXct("2007-01-01"), to=as.POSIXct("2007-01-30"),
length.out=200)
tt <- as.numeric(t - t[1], units="hours")
u <- cos(tt*2*pi/240)
v <- sin(tt*2*pi/240)
stickplot(t, u, v)
}
\author{Dan Kelley}
\keyword{misc}
