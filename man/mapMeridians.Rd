\name{mapMeridians}

\alias{mapMeridians}

\title{Plot meridians on an existing map}

\description{Plot meridians (lines of constant latitude) on a existing map}

\usage{mapMeridians(lat, lty='solid', lwd=0.5*par('lwd'), col='lightgray', \dots)}

\arguments{
    \item{lat}{either a boolean indicating whether to draw a meridian grid, or
        a single number to indicate the increment in meridian latitudes, or a
        vector of latitudes at which to draw meridians.}

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
        latitudelim=c(30,60), longitudelim=c(-130,-50),
        proj="polyconic", orientation=c(90, -90, 0))
mapMeridians()
}
}

\author{Dan Kelley}
\keyword{hplot}
