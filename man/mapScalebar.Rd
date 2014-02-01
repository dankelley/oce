\name{mapScalebar}

\alias{mapScalebar}

\title{Draw a scalebar on an existing map}

\description{Draw a scalebar on an existing map}

\usage{mapScalebar(position=c("topleft"), length,
            lwd=4*par("lwd"), cex=1.2*par("cex"), col="black")
}

\arguments{

    \item{position}{position for the scalebar, as for \code{\link{legend}}
        (only one allowed at present)}

    \item{length}{length to indicate, in kilometres.  If not provided, a
        possibly reasonable choice is made.}

    \item{lwd}{scalebar line width}

    \item{col}{colour}
    
    \item{cex}{character size for label}

}

\details{This relies on the edges of the map being on the earth, and thus fails
    for cases in which the form of the globe does not fill the plot box.  Then
    again, it makes very little sense to draw a scale bar in such a case.}

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

\author{Dan Kelley}
\keyword{hplot}
