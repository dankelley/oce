\name{mapMeridians}

\alias{mapMeridians}

\title{Plot meridians on an existing map [DEPRECATED]}

\description{Plot meridians (lines of constant latitude) on a existing map.
    \emph{DEPRECATED--will be removed by end Sept 2014}}

\usage{mapMeridians(latitude, lty='solid', lwd=0.5*par('lwd'), col='darkgray', \dots)}

\arguments{
    \item{latitude}{either a boolean indicating whether to draw a meridian
        grid, or a vector of latitudes at which to draw meridians.}

    \item{lty}{line type}

    \item{lwd}{line width}

    \item{col}{colour}

    \item{\dots}{optional arguments passed to \code{\link{lines}}.}
}

\details{Meridians that will not fit in the plotting space are ignored.}

\section{Bugs}{This should use \code{\link{approx}} to fill in multiple
    segments within the line, so that e.g. great circle lines will be curved.}

\seealso{See \code{\link{mapPlot}} for general information on plotting maps,
    including other functions.}

\examples{
\dontrun{
library(oce)
data(coastlineWorld)
longitude <- coastlineWorld[['longitude']]
latitude <- coastlineWorld[['latitude']]
mapPlot(longitude, latitude, type='l',
longitudelim=c(-130,-50), latitudelim=c(30,60),
proj="polyconic", orientation=c(90, -90, 0))
mapMeridians()
}
}

\author{Dan Kelley (see \code{\link{lonlat2map}} for a note on the authors of
    PROJ.4).}


\keyword{hplot}

