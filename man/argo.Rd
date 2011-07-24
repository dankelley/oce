\name{argo}

\docType{data}

\alias{argo}

\title{ARGO drifter dataset}

\description{This is an ARGO drifter data object, for drifter 6900388,
    downloaded in July 2011.  These data were collected and made freely
    available by the International Argo Program and the national programs that
    contribute to it.  (\url{http://www.argo.ucsd.edu},
    \url{http://argo.jcommops.org}).  The Argo Program is part of the Global
    Ocean Observing System.}

\examples{
\dontrun{
# How data were created
argo <- read.drifter("/data/OAR/6900388_traj.nc")
}
library(oce)
data(argo)
summary(argo)
data(coastlineWorld)
plot(argo, which="trajectory", coastline=coastlineWorld)
}

\usage{data(argo)}

\source{\url{http://www.usgodae.org/ftp/outgoing/argo/dac/bodc/6900388}}

\seealso{See \code{\link{read.drifter}} for notes on the contents of
  \code{drifter} objects, and for functions to work with them.}

\author{Dan Kelley}

\keyword{datasets}
