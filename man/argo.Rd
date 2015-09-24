% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{argo}

\docType{data}

\alias{argo}

\title{ARGO drifter dataset}

\description{This is an ARGO drifter data object, for drifter 6900388,
  downloaded in July 2011.  These data were collected and made freely available
  by the International Argo Program and the national programs that contribute to
  it.  Salinity, temperature and pressure are each matrices with first index
  corresponding to depth, e.g.  \code{argo[["temperature"]][1,]} is the
  surface temperature.}

\examples{
\dontrun{
library(oce)
data(argo)
summary(argo)
data(coastlineWorld)
plot(argo, which="trajectory", coastline=coastlineWorld)
}
}

\usage{data(argo)}

\source{This is the profile stored in the file \code{6900388_prof.nc}
  downloaded from the \code{usgodae.org} website in March 2012.}
    
% 20150702: removed the following because usgodae.org works only sporadically:
% \url{http://www.usgodae.org/ftp/outgoing/argo/dac/bodc/6900388/6900388_prof.nc}

\seealso{See \code{\link{argo-class}} for notes on the contents of
  \code{argo} objects, and for functions to work with them.}

\author{Dan Kelley}

\keyword{datasets}
