\name{interp.barnes}

\alias{interp.barnes}

\title{Grid data using Barnes algorithm}

\description{Grid data using Barnes algorithm.}

\usage{gridded <- interp.barnes(x, y, z, w=NULL, xg=NULL, yg=NULL, xr=NULL, yr=NULL, 
gamma=0.5, iterations=2)}

\arguments{
  \item{x}{a vector of x locations.}
  \item{y}{a vector of y locations.}
  \item{z}{a vector of z values, one at each (x,y) location.}
  \item{w}{a optional vector of weights at the (x,y) location.  If not
    supplied, i.e. if a NULL value, then a weight of 1 is used for each
    point, which means equal weighting.  Higher weights give data points
    more influence.}
  \item{xg}{an optional vector defining the x grid.  If not supplied,
    this will be determined from the x data, using \code{pretty(x,
      n=50)}.}
  \item{yg}{a optional vector defining the y grid.  If not supplied,
    this will be determined from the x data, using \code{pretty(y,
      n=50)}.  In the special case with all y values identical,
    \code{yg} is set to that value.  This is useful for one-dimensional
  gridding, e.g. for profiles (see below).}
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

# Contouring example, with wind-speed data from Koch et al. (1983)
data(wind)
u <- interp.barnes(wind$x, wind$y, wind$u)
contour(u$xg, u$yg, u$zg, col="red", labcex=1)
text(wind$x, wind$y, wind$u, cex=0.7, col="blue")

# One-dimensional example, smoothing a salinity profile
data(ctd)
p <- ctd$data$pressure
y <- rep(1, length(p)) # fake y data, with arbitrary value
S <- ctd$data$salinity
pg <- pretty(p, n=100)
g <- interp.barnes(p, y, S, xg=pg, xr=2)
plot(S, p, cex=0.5, col="blue", ylim=rev(range(p)))
lines(g$zg, g$xg, col="red")
}

\references{S. E.  Koch and M.  DesJardins and P. J. Kocin, 1983.  ``An
  interactive Barnes objective map anlaysis scheme for use with
  satellite and conventional data,'' \emph{J.  Climate Appl.  Met.}, vol
  22, p. 1487-1503.}

\author{Dan Kelley}

\keyword{misc}

\concept{tide}
