\name{binApply}

\alias{binApply}
\alias{binApply1D}
\alias{binApply2D}

\title{Apply a function to binned matrix data}

\description{Apply a function to binned matrix data}

\usage{binApply1D(x, f, xbreaks, FUN, \dots)
binApply2D(x, y, f, xbreaks, ybreaks, FUN, \dots)}

\arguments{
  \item{x}{a vector of numerical values.}
  
  \item{y}{a vector of numerical values.}

  \item{f}{a vector of data to which the elements of \code{FUN} may be
      supplied}

  \item{xbreaks}{values of x at the boundaries between bins; calculated using
      \code{\link{pretty}} if not supplied.}

  \item{ybreaks}{values of y at the boundaries between bins; calculated using
      \code{\link{pretty}} if not supplied.}

  \item{FUN}{function to apply to the data}
   
  \item{\dots}{arguments to pass to the function \code{FUN}}

}

\details{The function \code{FUN} is applied to \code{f} in bins specified by
    \code{xbreaks} and \code{ybreaks}.  (If \code{FUN} is \code{\link{mean}},
    consider using \code{\link{binMean2D}} instead, since it should be
    faster.)}

\value{A list with the following elements: the breaks in x and y
    (\code{xbreaks} and \code{ybreaks}), the break mid-points (\code{xmids} and
    \code{ymids}), and a matrix containing the result of applying function
    \code{FUN} to \code{f} subsetted by these breaks.}

\examples{
library(oce)
## (a) 1D: salinity profile with median and quartile 1 and 3
data(ctd)
p <- ctd[["pressure"]]
S <- ctd[["salinity"]]
q1 <- binApply1D(p, S, pretty(p, 30), function(x) quantile(x, 1/4))
q3 <- binApply1D(p, S, pretty(p, 30), function(x) quantile(x, 3/4))
plotProfile(ctd, "salinity", col='gray', type='n')
polygon(c(q1$result, rev(q3$result)),
c(q1$xmids, rev(q1$xmids)), col='gray')
points(S, p, pch=20)

## (b) 2D: secchi depths in lat and lon bins
if (require(ocedata)) {
    data(secchi, package="ocedata")
    col <- rev(oce.colorsJet(100))[rescale(secchi$depth,
                                           xlow=0, xhigh=20,
                                           rlow=1, rhigh=100)]
    zlim <- c(0, 20)
    breaksPalette <- seq(min(zlim), max(zlim), 1)
    colPalette <- rev(oce.colorsJet(length(breaksPalette)-1))
    drawPalette(zlim, "Secchi Depth", breaksPalette, colPalette)
    data(coastlineWorld)
    mapPlot(coastlineWorld, longitudelim=c(-5,20), latitudelim=c(50,66),
      grid=5, fill='gray', projection="+proj=lcc +lat_1=50 +lat_2=65")
    bc <- binApply2D(secchi$longitude, secchi$latitude,
                     pretty(secchi$longitude, 80),
                     pretty(secchi$latitude, 40),
                     f=secchi$depth, FUN=mean)
    mapImage(bc$xmids, bc$ymids, bc$result, zlim=zlim, col=colPalette)
    mapPolygon(coastlineWorld, col='gray')
}
}

\author{Dan Kelley}

\keyword{misc}

