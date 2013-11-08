\name{binApply}

\alias{binApply}
\alias{binApply2D}

\title{Apply a function to binned matrix data}

\description{Apply a function to binned matrix data}

\usage{binApply2D(x, y, data, xbreaks, ybreaks, FUN)}

\arguments{
  \item{x}{a vector of numerical values.}
  
  \item{y}{a vector of numerical values.}

  \item{data}{a vector of data to which the elements of \code{FUN} may be
      supplied}

  \item{xbreaks}{values of x at the boundaries between bins; calculated using
      \code{\link{pretty}} if not supplied.}

  \item{ybreaks}{values of y at the boundaries between bins; calculated using
      \code{\link{pretty}} if not supplied.}

  \item{FUN}{function to apply to data}

}

\details{The function \code{FUN} is applied to \code{data} in bins specified
    by \code{xbreaks} and \code{ybreaks}.  Warning: this function is slow; if
    all that is required is an average value, use \code{\link{binMean2D}}.}

\value{A list with the following elements: the breaks in x and y
    (\code{xbreaks} and \code{ybreaks}), the break mid-points (\code{xmids} and
    \code{ymids}), and a matrix containing the result of applying function
    \code{FUN} to data subsetted by these breaks.}

\examples{
\dontrun{
library(oce)
mapPlot(coastlineWorld, longitudelim=c(-5, 20), latitudelim=c(50, 66),
        grid=5, fill='gray', proj="lambert",
        parameters=c(lat0=50, lat1=65))
data(secchi)

col <- rev(oceColorsJet(100))[rescale(secchi$depth,xlow=0,xhigh=20,rlow=1,rhigh=100)]
#mapPolygon(coastlineWorld, col='gray')

zlim <- c(0, 20)
breaksPalette <- seq(min(zlim), max(zlim), 1)
colPalette <- rev(oceColorsJet(length(breaksPalette)-1))
bc <- binApply2D(secchi$longitude, secchi$latitude,
                 pretty(secchi$longitude, 80),
                 pretty(secchi$latitude, 40),
                 data=secchi$depth, func=mean)
mapImage(bc$xmids, bc$ymids, bc$result, zlim=zlim, col=colPalette)
mapPolygon(coastlineWorld, col='gray')
}
}


\author{Dan Kelley}

\keyword{misc}
