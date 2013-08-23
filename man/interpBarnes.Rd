\name{interpBarnes}

\alias{interpBarnes}

\title{Grid data using Barnes algorithm}

\description{Grid data using Barnes algorithm.}

\usage{interpBarnes(x, y, z, w,
    xg, yg, xgl, ygl,
    xr, yr, gamma=0.5, iterations=2, trim=0,
    debug=getOption("oceDebug"))}

\arguments{
  \item{x}{a vector of x locations.}
  \item{y}{a vector of y locations.}
  \item{z}{a vector of z values, one at each (x,y) location.}
  \item{w}{a optional vector of weights at the (x,y) location.  If not
    supplied, i.e. if a NULL value, then a weight of 1 is used for each
    point, which means equal weighting.  Higher weights give data points
    more influence.}
  \item{xg}{optional vector defining the x grid.  If not supplied, this will
      be determined from the x data, using \code{pretty(x, n=50)}.}
  \item{yg}{analogous to \code{xg}.}
  \item{xgl}{optional length of x grid, to be constructed with
      \code{\link{seq}} spanning the range of \code{x}.  The 
      value of \code{xgl} is only examined if \code{xg} is not
      supplied.}
  \item{ygl}{analogous to \code{xgl}.}
  \item{xr}{a optional value defining the width of the radius ellipse in
    the x direction.  If not supplied, }
  \item{yr}{an optional value defining the height of the radius ellipse
    in the y direction.}
  \item{gamma}{grid-focussing parameter.  At each iteration, \code{xr}
    and \code{yr} are reduced by a factor of \code{sqrt(gamma)}.}
  \item{iterations}{number of iterations.}

  \item{trim}{a number between 0 and 1, indicating the quantile of data weight
      to be used as a criterion for blanking out the gridded value (using
      \code{NA}).  If 0, the whole \code{zg} grid is returned.  If >0, any
      spots on the grid where the data weight is less than the \code{trim}-th
      \code{\link{quantile}} are set to \code{NA}.  See examples.}

  \item{debug}{a flag that turns on debugging.  Set to 0 for no debugging
      information, to 1 for more, etc; the value is reduced by 1 for each
      descendent function call.}
}

\details{The algorithm follows that described by Koch et al. (1983), with the
    addition of the ability to blank out the grid in spots where data are
    sparse, using the \code{trim} argument.}

\value{A list containing \code{xg} (a vector holding the x-grid), \code{yg}) (a
    vector holding the y-grid), \code{zg} (a matrix holding the gridded values)
    and \code{wg} (a matrix holding the weights used in the interpolation at
    its final iteration).}

\examples{
library(oce)

# Contouring example, with wind-speed data from Koch et al. (1983)
data(wind)
u <- interpBarnes(wind$x, wind$y, wind$z)
contour(u$xg, u$yg, u$zg, labcex=1)
text(wind$x, wind$y, wind$z, cex=0.7, col="blue")

# As above, but blank out spots where data are sparse
u <- interpBarnes(wind$x, wind$y, wind$z, trim=0.1)
contour(u$xg, u$yg, u$zg, level=seq(0, 30, 1)) 
points(wind$x, wind$y, cex=1.5, pch=20, col="blue")

# One-dimensional example, smoothing a salinity profile
data(ctd)
p <- pressure(ctd)
y <- rep(1, length(p)) # fake y data, with arbitrary value
S <- salinity(ctd)
pg <- pretty(p, n=100)
g <- interpBarnes(p, y, S, xg=pg, xr=1)
plot(S, p, cex=0.5, col="blue", ylim=rev(range(p)))
lines(g$zg, g$xg, col="red")
}

\references{S. E.  Koch and M.  DesJardins and P. J. Kocin, 1983.  ``An
  interactive Barnes objective map anlaysis scheme for use with
  satellite and conventional data,'' \emph{J.  Climate Appl.  Met.}, vol
  22, p. 1487-1503.}

\seealso{See \code{\link[ocedata]{wind}}.}

\author{Dan Kelley}

\keyword{misc}

\concept{tide}
