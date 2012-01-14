\name{schmitt}

\docType{data}

\alias{schmitt}

\title{Schmitt's (1981) NACW temperature-salinity data}

\description{Schmitt's (1981) temperature-salinity data for North Atlantic
    Central Water.}

\usage{data(schmitt)}

\examples{
library(oce)
data(schmitt)
plotTS(as.ctd(schmitt$S, schmitt$theta, 0))
}

\source{Data were digitized from Schmitt's Figure 1, to a tolerance of about
    half the symbol size in that diagram.  Schmitt reported the original data
    source as a cruise of the vessel T. G. Thompson in 1970, and a location of
    25.2N and 35.8W.}

\references{
    R. W. Schmitt, 1981.  Form of the temperature-salinity relationship in
    central water: evidence for double-diffusive mixing.  \emph{Journal of
        Physical Oceanography}, 11:1015-1026.
}

\keyword{datasets}
