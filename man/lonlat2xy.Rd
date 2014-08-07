\name{lonlat2xy}

\alias{lonlat2xy}

\title{Convert from longitude and latitude to x-y coordinates}

\description{Convert from longitude and latitude to x-y coordinates}

\usage{lonlat2xy(longitude, latitude,
          projection="", parameters=NULL, orientation=NULL)}

\arguments{

  \item{longitude}{a vector containing decimal longitudes, or a list containing
      items named \code{longitude} and \code{latitude}, in which case the
      indicated values are used, and next argument is ignored.}

  \item{latitude}{a vector containing decimal latitude (ignored if
      \code{longitude} is a list, as described above).}

  \item{projection}{optional indication of projection.  This may be in the
      format used by the \code{proj4} package (i.e. a string starting with
      \code{+proj=}) or a string naming a projection according to the format
      used by the \code{mapproj} package.  In the \code{proj4} case, the
      \code{parameters} and \code{orientation} arguments are ignored.}
  
  \item{parameters,orientation}{arguments to be supplied to
      \code{\link{mapproject}}, if a \code{mapproj} style \code{projection} has
      been specified.}

}

\details{If a projection is already being used (e.g. as set by
    \code{\link{mapPlot}}) then only \code{longitude} and \code{latitude}
    should be given, and the other arguments will be inferred by
    \code{lonlat2xy}.  This is important because otherwise, if a new projection
    is called for, it will ruin any additions to the existing plot.}

\value{A list containing \code{x} and \code{y}.  These values have a clear
    meaning for \code{proj4}-style projections but not for \code{mapproj}-style
    projections.}

\examples{
library(oce)
## Cape Split, in the Minas Basin of the Bay of Fundy
cs <- list(longitude=-64.49657,latitude=45.33462)
xy <- lonlat2xy(cs, projection="+proj=merc")
xy2lonlat(xy)
xy <- lonlat2xy(cs, projection="mercator")
xy2lonlat(xy)
}

\seealso{\code{mapLongitudeLatitudeXY} is a safer alternative, if a map has
    already been drawn with \code{\link{mapPlot}}, because that function cannot
    alter an existing projection.}

\author{Dan Kelley}

\keyword{misc}

