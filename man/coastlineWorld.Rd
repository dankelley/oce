\name{coastlineWorld}

\docType{data}

\alias{coastlineWorld}

\title{World coastline}

\description{World coastline, in any of three resolutions}

\usage{data(coastlineWorld)}

\details{In each case, the longitudes are in the range from -180 to 180 degrees, i.e.
    western longitudes have negative values.  Large lakes (particularly the Great Lakes)
    are missing from these datasets, since the intention is for use in ocean mapping.
    The resolutions of the three coastlines are listed below, along with
    typical applications.
    \itemize{

        \item \code{coastlineWorld} is a coarse resolution 1:110M (with 10,696
        points), suitable for world-scale plots plotted at a small size, e.g.
        inset diagrams

        \item \code{coastlineWorldMedium} resolution 1:50M (with 100,954 points),
        suitable for world- or basin-scale plots

        \item \code{coastlineWorldFine} resolution 1:10M (with 552,670 points),
        suitable for shelf-scale plots

    }
}

\source{Downloaded from \url{http://www.naturalearthdata.com}, in
    \code{ne_110m_admin_0_countries.shp}.}

\seealso{The \CRANpkg{ocedata} package provides two more coastlines with better
    resolution: \code{coastlineWorldMedium} and \code{coastlineWorldFine}.
    
    The documentation for \code{\link{coastline-class}} explains the structure
    of \code{coastline} objects and discusses functions that deal with them.

    The \code{maps} package provides a database named \code{world} that has
    27221 points.}

\author{Dan Kelley}

\keyword{datasets}

