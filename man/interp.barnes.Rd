\name{interp.barnes}

\alias{interp.barnes}

\title{Grid data using Barnes algorithm}

\description{Grid data using Barnes algorithm.}

\usage{interp.barnes <- function(x, y, z, w, xg, yg, xr, yr, gamma=0.5, iterations=2)}

\arguments{
  \item{x}{a vector of x locations.}
  \item{y}{a vector of y locations.}
  \item{z}{a vector of z values, one at each (x,y) location.}
  \item{w}{a optional vector of weights at the (x,y) location.  If not
    supplied, a weight of 1 is used for each point, which means equal
    weighting.  Higher weights give data points more influence.}
  \item{xg}{an optional vector defining the x grid.  If not supplied,
    this will be determined from the x data, using \code{pretty(x,
      n=30)}.}
  \item{yg}{a optional vector defining the y grid.  If not supplied,
    this will be determined from the x data, using \code{pretty(y,
      n=30)}.}
  \item{xr}{a optional value defining the width of the radius ellipse in
    the x direction.  If not supplied, }
  \item{yr}{an optional value defining the height of the radius ellipse
    in the y direction.}
  \item{gamma}{grid-focussing parameter.  At each iteration, \code{xr}
    and \code{yr} are reduced by a factor of \code{sqrt(gamma)}.}
  \item{iterations}{number of iterations.}
}

\details{The algorithm follows that described by Koch et al. (1983).}

\value{A list containing \code{xg}, the x-grid, \code{yg}, the y-grid,
  and \code{zg}, the gridded value.}

\examples{
library(oce)
data(wind)
u <- interp.barnes(wind$x, wind$y, wind$u)
contour(u$xg, u$yg, u$zg)
points(wind$x, wind$y)
}

\references{S. E.  Koch and M.  DesJardins and P. J. Kocin, 1983.  ``An
interactive Barnes objective map anlaysis scheme for use with satellite
and conventional data,'' \emph{J.  Climate Appl.  Met.}, vol 22,
p. 1487-1503.}

\author{Dan Kelley}

\keyword{misc}

\concept{tide}
