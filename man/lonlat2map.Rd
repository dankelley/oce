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
      with \code{+proj=}. (See the \dQuote{Deprecation notice} for
      \code{\link{mapPlot}}.)}
  
  \item{parameters,orientation}{See \dQuote{Deprecation notice} for
      \code{\link{mapPlot}}.} }

\details{If a projection is already being used (e.g. as set by
    \code{\link{mapPlot}}) then only \code{longitude} and \code{latitude}
    should be given, and the other arguments will be inferred by
    \code{lonlat2map}.  This is important because otherwise, if a new projection
    is called for, it will ruin any additions to the existing plot.
}

\section{Deprecation notice}{At the moment of writing, it is still also
    possible to specify projections using \code{projection}, \code{parameters}
    and \code{orientation} in the terminology employed by the \code{mapproj}
    package. In that case, the calculations are done using that package (if it
    is installed). Unfortunately, this method can yield poor results, because
    inverse-projection is not handled by \code{mapproj}. For this reason, this
    capability is likely to be removed in a future version of \code{oce}. It is
    only provided in the present version to give users time to alter their
    code. It will be removed sometime in the year 2015.}

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

\author{Dan Kelley wrote the \code{lonlat2map()} and the C code that
    connects with PROJ.4 code. The PROJ.4 code was written by Gerald
    Evenden, Frank Warmerdam, Howard Butler, Brent Fraser, Chris Stuber,
    Craig Bruce, Victor Osipkov, Andrea Antonello, Charles Karney,
    Karsten Engsager, and Knud Poder.}

\keyword{misc}

