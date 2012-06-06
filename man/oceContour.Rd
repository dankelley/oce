\name{oceContour}

\alias{oceContour}

\title{Contour with ability to flip x and y}

\description{This provides something analagous to \code{\link{contour}}, but
    with the ability to flip x and y.}

\usage{oceContour(x, y, z, revx=FALSE, revy=FALSE, \dots)}

\arguments{
  \item{x}{values for x grid.}
  \item{y}{values for y grid.}
  \item{z}{matrix for values to be contoured.  The first dimension of \code{z}
      must equal the number of items in \code{x}, etc.}
  \item{revx}{set to \code{TRUE} to reverse the order in which the labels on
      the x axis are drawn}
  \item{revy}{set to \code{TRUE} to reverse the order in which the labels on
      the y axis are drawn}
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{It is common in oceanography to plot depth below the surface on the y
    axis, but also to need to put surface data at the top of the graph, and
    setting \code{revy=TRUE} accomplishes this.}

\examples{
library(oce)
data(adp)
U <- adp[["v"]][,,1]
t <- adp[["time"]]
distance <- adp[["distance"]]
z <- distance - max(distance) 
depth <- abs(z)
par(mfrow=c(3,1), mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
contour(t,distance,U,levels=0, ylab="Distance")
oceContour(t,z,U,levels=0, ylab="z")
oceContour(t,depth,U,levels=0,revy=TRUE, ylab="depth")
}

\author{Dan Kelley}

\keyword{misc}
