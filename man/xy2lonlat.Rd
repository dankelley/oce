\name{xy2lonlat}

\alias{xy2lonlat}

\title{Convert from x-y to longitude and latitude}

\description{Convert from x-y coordinates to longitude and latitude}

\usage{xy2lonlat(x, y, projection="", parameters=NULL, orientation=NULL)}

\arguments{

  \item{x,y}{coordinates of points in the projected space.  These values have a
      clear meaning for \code{proj4}-style projections but not for
      \code{mapproj}-style projections.}

  \item{projection}{either a projection name as used by the \code{mapproj}
      package (in which case \code{parameters} and \code{orientation} are
      supplied to \code{\link{mapproject}}, or a projection specification as
      used by the \code{proj4} package (in which case \code{parameters} and
      \code{orientation} are ignored).}

  \item{parameters,orientation}{arguments to be supplied to
      \code{\link{mapproject}}, if a \code{mapproj} style \code{projection} has
      been specified.}
}

\details{If a projection is already being used (e.g. as set by
    \code{\link{mapPlot}}) then only \code{longitude} and \code{latitude}
    should be given, and the other arguments will be inferred by
    \code{lonlat2xy}.  This is important because otherwise, if a new projection
    is called for, it will ruin any additions to the existing plot.

    The accuracy of \code{xy2lonlat} is higher for \code{proj4}-style projections
    than for \code{mapproj}-style projections.  This is because the
    \code{proj4} package provides inverse functions, while \code{mapproj} does
    not.  In the latter case, \code{xy2lonlat} attempts to invert the
    projection by a minimization procedure using \code{\link{optim}}, and this
    method can be quite inaccurate, depending on the projection and the
    location in question.}

\value{A list containing \code{longitude} and \code{latitude}.}

\examples{
    \dontrun{
library(oce)
## Cape Split, in the Minas Basin of the Bay of Fundy
cs <- list(longitude=-64.49657,latitude=45.33462)
xy <- lonlat2xy(cs, projection="+proj=merc")
xy2lonlat(xy, projection="+proj=merc")
xy <- lonlat2xy(cs, projection="mercator")
xy

}
}

\seealso{\code{\link{lonlat2xy}} does the inverse operation.  For general
    projections and their inverses, use \code{\link{lonlat2xy}} and
    \code{\link{xy2lonlat}}.}

\references{\url{http://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system},
    downloaded May 31, 2014.}

\author{Dan Kelley}

\keyword{misc}

