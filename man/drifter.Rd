% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{drifter}

\docType{data}

\alias{drifter}

\title{ARGO drifter dataset}

\description{This is an ARGO drifter data object, for drifter 6900388,
  downloaded in July 2011.  These data were collected and made freely available
  by the International Argo Program and the national programs that contribute to
  it.  Salinity, temperature and pressure are each matrices with first index
  corresponding to depth, e.g.  \code{drifter[["temperature"]][1,]} is the
  surface temperature.}

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

\source{This is the profile stored in the file \code{6900388_prof.nc}
  downloaded from the \code{usgodae.org} website in March 2012.}
    
% 20150702: removed the following because usgodae.org works only sporadically:
% \url{http://www.usgodae.org/ftp/outgoing/argo/dac/bodc/6900388/6900388_prof.nc}

\seealso{See \code{\link{drifter-class}} for notes on the contents of
  \code{drifter} objects, and for functions to work with them.}

\author{Dan Kelley}

\keyword{datasets}
