\name{mapPlot}

\alias{mapPlot}

\title{Plot a map}

\description{Plot a map}

\usage{mapPlot(longitude, latitude, longitudelim, latitudelim, grid=TRUE,
    bg, fill=NULL, type='l', axes=TRUE, drawBox=TRUE, showHemi=TRUE,
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
        the object and the second argument to this function is ignored.  If
        \code{longitude} is missing, it is set to
        \code{\link{coastlineWorld}}.}

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

    \item{showHemi}{logical value indicating whether to show the hemisphere in
        axis tick labels.}

    \item{polarCircle}{a number indicating the number of degrees of latitude
        extending from the poles, within which zones are not drawn.}

    \item{projection}{a string indicating the form of projection to be used.
        This may be in either of two formats.  The first is that used by
        \code{\link[mapproj]{mapproject}} in the \code{mapproj} package; if
        this is used, then the next two arguments are also used.  The second
        form is that used by \code{\link[proj4]{project}} in the \code{proj4}
        package.  See \dQuote{Examples}.}

    \item{parameters}{parameters, if \code{projection} is in \code{mapproj}
        form; see \code{\link[mapproj]{mapproject}}.}
    
    \item{orientation}{orientation, if \code{projection} is in \code{mapproj}
        form; see \code{\link[mapproj]{mapproject}}.}

    \item{debug}{a flag that turns on debugging.  Set to 1 to get a moderate
        amount of debugging information, or to 2 to get more.} 

    \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Creates a map using the indicated projection.  The available
    projections can be found in the documentation for
    \code{\link[mapproj]{mapproject}}, which also provides information on the
    arguments named \code{parameters} and \code{orientation}.  Further details
    on these and other projections are provided by Snyder (1987), an exhaustive
    treatment that includes many illustrations, an overview of the history of
    the topic, and some notes on the strengths and weaknesses of the various
    formulations.  See especially pages 2 through 7, which define terms and
    provide recommendations.}

\seealso{Points may be added to a map with \code{\link{mapPoints}}, lines with
    \code{\link{mapLines}}, and text with \code{\link{mapText}}.  Points on a
    map may be found with \code{\link{mapLocator}}.  Great circle paths can be
    calculated with \code{\link{geodGc}}.  Scale bars may be added with
    \code{\link{mapScalebar}}.}

\examples{
\dontrun{
library(oce)
data(coastlineWorld)

## Mollweide is an equal-area projection that works well for
## whole-globe views, below shown in default and Pacific-focus
## views.  See Snyder (1987 page 54).
mapPlot(coastlineWorld, proj="mollweide")
mapPlot(coastlineWorld, proj="mollweide", orientation=c(90,-180,0))

## proj4 form
mapPlot(coastlineWorld, proj="+proj=moll")

## Orthographic projections resemble a globe, making them
## attractive for non-technical use, but they are neither conformal
## nor equal-area, so they are somewhat limited for serious
## use on large scales.  See Snyder (1987 section 20).  The
## examples given below illustrate use for low-latitude
## Pacific and high-latitude Atlantic/Arctic.
par(mfrow=c(1,2), mar=rep(1, 4))
mapPlot(coastlineWorld, projection="orthographic",
        orientation=c(0,-180,0))
mapPlot(coastlineWorld, projection="orthographic",
        orientation=c(60,-40,0))
mtext("BUG: this panel should use proj4", font=2, col="purple")

## The Lambert conformal conic projection is an equal-area
## projection recommended by Snyder for regions of large 
## east-west extent away from the equator, here illustrated 
## for the USA and Canada.  Readers should compare the results
## with those with a polygonic projection, which is also popular.
mapPlot(coastlineWorld, longitudelim=c(-130,-55), latitudelim=c(35,60),
        proj="lambert", parameters=c(lat0=40,lat1=60),
        orientation=c(90,-100,0))

## The stereographic projection (Snyder 1987 page 120) is conformal,
## used below for an Arctic view with a Canadian focus.  Note the trick
## of going past the pole: the second latitudelim value is
## 180 minus the first, and the second longitudelim is 180 plus
## the first; this uses image points "over" the pole.
par(mfrow=c(1,2))
mapPlot(coastlineWorld, longitudelim=c(-130,50), latitudelim=c(70,110),
        proj="stereographic", orientation=c(90, -135, 0), fill='gray') 
mapPlot(coastlineWorld, longitudelim=c(-130,50), latitudelim=c(70,110),
        proj="+proj=stere +lat_0=90 +lon_0=-135") # BUG: filling fails
mtext("BUG: filling does not work yet in proj4 case", col='purple', font=2)
mtext("BUG: axes do not work in proj4 case", line=1, col='purple', font=2)

}
}

\author{Dan Kelley}

\references{Snyder, John P., 1987.  Map Projections: A Working Manual.  USGS
    Professional Paper: 1395 (available at
    \url{pubs.usgs.gov/pp/1395/report.pdf}). 

    Jenny, B., 2012.  Adaptive composite map projections.  IEEE Transactions on
    Visualization and Computer Graphics (Proceedings Scientific Visualization /
    Information Visualization 2012), 18-22, p 2575-2582.

    XKCD guide to map projections (\url{http://xkcd.com/977/}).
}

\keyword{hplot}

