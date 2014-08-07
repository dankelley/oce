\name{map2lonlat}

\alias{map2lonlat}

\title{Convert from x-y to longitude and latitude}

\description{Convert from x-y coordinates to longitude and latitude}

\usage{map2lonlat(x, y)}

\arguments{

  \item{x}{a vector containing the x component of points in the projected
      space, or a list containing items named \code{x} and \code{y}, in which
      case the next argument is ignored.}

  \item{y}{a vector containing the y coordinate of points in the projected
      space (ignored if \code{x} is a list, as described above).}

}

\details{A projection must already have been set up, by a call to
    \code{\link{mapPlot}} or \code{\link{lonlat2map}}.
  
    The values of \code{x} and \code{y} have a clear meaning for
    \code{proj4}-style projections.  This is not true for \code{mapproj}-style
    projections, since \code{\link{mapproject}} shifts the \code{x} component
    of its return value to have mean value equal to 0, up to a calculation
    error. 

    The accuracy of \code{map2lonlat} tends to be higher for \code{proj4}-style
    projections than for \code{mapproj}-style projections.  This is because the
    \code{proj4} package provides inverse functions, while \code{mapproj} does
    not.  In the latter case, \code{map2lonlat} attempts to invert the
    projection by a minimization procedure using \code{\link{optim}}, and this
    method can produce inaccurate results (or simply fail), depending on the
    projection and the location in question.}

\value{A list containing \code{longitude} and \code{latitude}.}

\examples{
library(oce)
## Cape Split, in the Minas Basin of the Bay of Fundy
cs <- list(longitude=-64.49657,latitude=45.33462)
xy <- lonlat2map(cs, projection="+proj=merc")
map2lonlat(xy)
xy <- lonlat2map(cs, projection="mercator")
map2lonlat(xy)
}

\seealso{\code{\link{lonlat2map}} does the inverse operation.}

\references{\url{http://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system},
    downloaded May 31, 2014.}

\author{Dan Kelley}

\keyword{misc}

