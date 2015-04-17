\name{lonlat2map}

\alias{lonlat2map}

\title{Convert from longitude and latitude to x-y coordinates}

\description{Convert from longitude and latitude to x-y coordinates}

\usage{lonlat2map(longitude, latitude,
          projection="", parameters=NULL, orientation=NULL)}

\arguments{

  \item{longitude}{a vector containing decimal longitudes, or a list containing
      items named \code{longitude} and \code{latitude}, in which case the
      indicated values are used, and next argument is ignored.}

  \item{latitude}{a vector containing decimal latitude (ignored if
      \code{longitude} is a list, as described above).}

  \item{projection}{optional indication of projection.  This is normally a
      string in the format used by the \code{proj4} package, i.e.  starting
      with \code{+proj=}. (In a feature to be removed soon, this may also be a
      string in the format used by the \code{mapproj} package, in which case
      the next two parameters may also be specified, but see the deprecation
      notice in \sQuote{Details}.)}
  
  \item{parameters,orientation}{arguments to be supplied to \code{mapproject}
      in the \code{mapproj} package, if a \code{mapproj} style
      \code{projection} has been specified. (See deprecation notice in
      \sQuote{Details}.)}

}

\details{If a projection is already being used (e.g. as set by
    \code{\link{mapPlot}}) then only \code{longitude} and \code{latitude}
    should be given, and the other arguments will be inferred by
    \code{lonlat2map}.  This is important because otherwise, if a new projection
    is called for, it will ruin any additions to the existing plot.

    \strong{Deprecation notice.} The \code{mapproj}-style specification of
    projections is an artifact from an early version of \code{oce}, and it may
    be removed at any time without notice.

}

\value{A list containing \code{x} and \code{y}.  These values have a clear
    meaning for \code{proj4}-style projections but not for \code{mapproj}-style
    projections.}

\examples{
library(oce)
## Cape Split, in the Minas Basin of the Bay of Fundy
cs <- list(longitude=-64.49657,latitude=45.33462)
xy <- lonlat2map(cs, projection="+proj=merc")
map2lonlat(xy)
}

\seealso{\code{mapLongitudeLatitudeXY} is a safer alternative, if a map has
    already been drawn with \code{\link{mapPlot}}, because that function cannot
    alter an existing projection.}

\author{Dan Kelley}

\keyword{misc}

