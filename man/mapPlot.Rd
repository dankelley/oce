\name{mapPlot}

\alias{mapPlot}

\title{Plot a map}

\description{Plot a map}

\usage{mapPlot(longitude, latitude, longitudelim, latitudelim, grid=TRUE,
    bg, fill=NULL, type='l', axes=TRUE, drawBox=TRUE,
    polarCircle=0,
    projection="mollweide", parameters=NULL, orientation=NULL,
    debug=getOption("oceDebug"),
    \dots)
}

\arguments{

    \item{longitude}{longitudes of points to be plotted, or an object with a
        slot named \code{data} that contains items named \code{longitude} and
        \code{latitude} (e.g. of class \code{coastline}; see
        \code{\link{coastline-class}}), in which case position is inferred from
        the object and the second argument to this function is ignored.}

    \item{latitude}{latitudes of points to be plotted}

    \item{longitudelim}{optional limit of longitudes to plot}

    \item{latitudelim}{optional limit of latitudes to plot}

    \item{grid}{either a number (or pair of numbers) indicating the spacing of
        longitude and latitude lines, in degrees, or a logical value indicating
        whether to draw a default grid with 15 degree spacing.}

    \item{bg}{background colour for plot (ignored at present).}

    \item{fill}{colour to be used to fill land regions, or \code{NULL} to avoid
        filling.  For some map projections (particularly if the view includes
        the dateline), the filled region may not line up with coastlines,
        making it sensible to set \code{fill=NULL}; see \dQuote{Examples}.}

    \item{type}{value to indicate type of plot, as with
        \code{\link{par}("plot")}.}

    \item{axes}{logical value indicating whether to draw longitude and latitude
        values in the lower and left margin, respectively.  This may not work
        well for some projections or scales.}

    \item{drawBox}{logical value indicating whether to draw a box around the
        plot.  This is helpful for many projections at sub-global scale.}

    \item{polarCircle}{a number indicating the number of degrees of latitude
        extending from the poles, within which zones are not drawn.}

    \item{projection}{projection; see \code{\link{mapproject}}.}

    \item{parameters}{parameters for projection; see \code{\link{mapproject}}.}
    
    \item{orientation}{orientation for projection; see
        \code{\link{mapproject}}.}

    \item{debug}{a flag that turns on debugging.  Set to 1 to get a moderate
        amount of debugging information, or to 2 to get more.} 

    \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Creates a map using the indicated projection.}

\seealso{Points may be added to a map with \code{\link{mapPoints}}, lines may
    be added with \code{\link{mapLines}}, and text with \code{\link{mapText}}.
    Points on a map may be found with \code{\link{mapLocator}}.}

\examples{
\dontrun{
library(oce)
data(coastlineWorld)
longitude <- coastlineWorld[['longitude']]
latitude <- coastlineWorld[['latitude']]
mapPlot(longitude, latitude, type='l',
        latitudelim=c(0,120), longitudelim=c(-80,10),
        projection="orthographic", orientation=c(45,-100,0))

## Illustrate the Lambert conformal conic projection,
## said by Snyder (page 34) to be suitable for regions
## of large east-west extent away from the equator.
## Here, show USA and Canada.
mapPlot(coastlineWorld, type='l',
        latitudelim=c(30,60), longitudelim=c(-130,-50),
        proj="lambert", fill='lightgray',
        parameters=c(lat0=40,lat1=60),
        orientation=c(90,-90,0), grid=TRUE)

## Similar looking map, with the polyconic projection
mapPlot(coastlineWorld, type='l',
        latitudelim=c(30,60), longitudelim=c(-130,-50),
        proj="polyconic", orientation=c(90, -90,0), grid=TRUE)
}
}

\author{Dan Kelley}

\references{Snyder, John P., 1987.  Map Projections: A Working Manual.  USGS
    Professional Paper: 1395 (available at
    \url{pubs.usgs.gov/pp/1395/report.pdf}). 

    XKCD guide to map projections (\url{http://xkcd.com/977/}).
}

\keyword{hplot}
