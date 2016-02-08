\name{ungrid}

\alias{ungrid}

\title{Extract (x, y, z) from (x, y, grid)}

\description{Extract the grid points from a grid, returning columns.}

\usage{ungrid(x, y, grid)}

\arguments{
    \item{x}{a vector holding the x coordinates of grid.}
    \item{y}{a vector holding the y coordinates of grid.}
    \item{grid}{a matrix holding the grid.}
}

\details{This is useful for e.g. gridding large datasets, in which the first
    step might be to use \code{\link{binMean2D}}, followed by
    \code{\link{interpBarnes}}.}

\value{A list containing three vectors: \code{x}, the grid x values, \code{y},
    the grid y values, and \code{grid}, the grid values.}
    
\examples{
library(oce)
data(wind)
u <- interpBarnes(wind$x, wind$y, wind$z)
contour(u$xg, u$yg, u$zg)
U <- ungrid(u$xg, u$yg, u$zg)
points(U$x, U$y, col=oce.colorsJet(100)[rescale(U$grid, rlow=1, rhigh=100)], pch=20)
}

\author{Dan Kelley}

\keyword{misc}

