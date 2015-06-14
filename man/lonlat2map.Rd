% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{lonlat2map}

\alias{lonlat2map}

\title{Convert from longitude and latitude to x-y coordinates}

\description{Convert from longitude and latitude to x-y coordinates}

\usage{lonlat2map(longitude, latitude, projection="")}

\arguments{

  \item{longitude}{a vector containing decimal longitudes, or a list containing
    items named \code{longitude} and \code{latitude}, in which case the
    indicated values are used, and next argument is ignored.}

  \item{latitude}{a vector containing decimal latitude (ignored if
    \code{longitude} is a list, as described above).}

  \item{projection}{optional indication of projection.  This must be character
    string in the format used by the \CRANpkg{rgdal} package; see
    \code{\link{mapPlot}}.)}
}
     
\details{If a projection is already being used (e.g. as set by
  \code{\link{mapPlot}}) then only \code{longitude} and \code{latitude} should
  be given, and the other arguments will be inferred by \code{lonlat2map}.  This
  is important because otherwise, if a new projection is called for, it will
  ruin any additions to the existing plot.  }

\value{A list containing \code{x} and \code{y}.}

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

