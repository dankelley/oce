\name{drifter}

\docType{data}

\alias{drifter}

\title{ARGO drifter dataset}

\description{This is an ARGO drifter data object, for drifter 6900388,
    downloaded in July 2011.  These data were collected and made freely
    available by the International Argo Program and the national programs that
    contribute to it.  Salinity, temperature and pressure are each matrices
    with first index corresponding to depth, e.g.
    \code{drifter[["temperature"]][1,]} is the surface temperature.}

\examples{
\dontrun{
library(oce)
data(drifter)
summary(drifter)
data(coastlineWorld)
plot(drifter, which="trajectory", coastline=coastlineWorld)
}
}

\usage{data(drifter)}

\source{\url{http://www.usgodae.org/ftp/outgoing/argo/dac/bodc/6900388/6900388_prof.nc}}

\seealso{See \code{drifter-class} in the Oce package for notes on the contents
    of \code{drifter} objects, and for functions to work with them.}

\author{Dan Kelley}

\keyword{datasets}
