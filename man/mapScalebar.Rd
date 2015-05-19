\name{mapScalebar}

\alias{mapScalebar}

\title{Draw a scalebar on an existing map}

\description{Draw a scalebar on an existing map}

\usage{mapScalebar(x, y=NULL, length,
            lwd=1.5*par("lwd"), cex=par("cex"), col="black")
}

\arguments{

    \item{x, y}{position of the scalebar.  Eventually this should be similar to
        the corresponding arguments in \code{\link{legend}}, but at the moment
        \code{y} must be \code{NULL} and \code{x} must be \code{"topleft"}.}

    \item{length}{length to indicate, in kilometres.  If not provided, a
        reasonable choice is made, based on the underlying map.}

    \item{lwd}{scalebar line width}

    \item{col}{scalebar colour}
    
    \item{cex}{scalebar character size}

}

\details{The scale is appropriate to the centre of the plot, and will become
    increasingly inaccurate away from that spot, with the error depending on
    the projection and the fraction of the earth that is shown.}

\seealso{See \code{\link{mapPlot}} for general information on plotting maps,
    including other functions.}

\examples{
\dontrun{
library(oce)
## Arctic Ocean
data(coastlineWorld)
latlim <- 90 + 25 * c(-1, 1)
lonlim <- c(-130, -50)
orientation <- c(90, -90, 0)
proj <- "stereographic"
fill <- "lightgray"
mapPlot(coastlineWorld, latitudelim=latlim, longitudelim=lonlim,
        proj=proj, orientation=orientation,
        axes=FALSE, fill=fill)
mapScalebar()
}
}

\author{Dan Kelley (see \code{\link{lonlat2map}} for a note on the authors of
    PROJ.4).}

\keyword{hplot}
