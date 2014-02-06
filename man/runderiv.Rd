\name{runderiv}

\alias{runderiv}

\title{Calculate dy/dx with running regression}

\description{Calculate dy/dx with running regression}

\usage{runderiv(x, y, L)}

\arguments{
    \item{x}{a vector holding the x coordinates of grid.}
    \item{y}{a vector holding the y coordinates of grid.}
    \item{L}{width of running window, in x units.}
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
U <- runderiv(u$xg, u$yg, u$zg)
points(U$x, U$y, col=oceColorsJet(100)[rescale(U$grid, rlow=1, rhigh=100)], pch=20)
}

\author{Dan Kelley}

\keyword{misc}

