\name{mapImage}

\alias{mapImage}

\title{Plot an image on a existing map}

\description{Plot an image on an existing map}

\usage{mapImage(longitude, latitude, z, zlim, zclip=FALSE,
         breaks, col, colormap, border=NA,
         lwd=par("lwd"), lty=par("lty"), missingColor=NA,
         filledContour=FALSE, gridder="binMean2D",
         debug=getOption("oceDebug"))}

\arguments{

    \item{longitude}{longitudes of grid lines}

    \item{latitude}{latitudes of grid lines}

    \item{z}{matrix for image}
    
    \item{zlim}{limit for z (colour)}
    
    \item{zclip}{logical, indicating whether to clip the colours by using the
        \code{missingColor} for out-of-range values.  If if \code{zlim} is
        given then its min and max set the range.  If \code{zlim} is not given
        but \code{breaks} is given, then the min and max of \code{breaks} sets
        the range.  If neither \code{zlim} nor \code{breaks} is given, clipping
        is not done, i.e. the action is as if \code{zclip} were \code{FALSE}.}

    \item{breaks}{the z values for breaks in the colour scheme.  If this is of
        length 1, the value indicates the desired number of breaks, which is
        supplied to \code{\link{pretty}}, in determining clean break points.}

    \item{col}{either a vector of colours corresponding to the breaks, of length
        1 plus the number of breaks, or a function specifying colours,
        e.g. \code{\link{oce.colorsJet}} for a rainbow.}

    \item{colormap}{optional colormap, as created by \code{\link{colormap}}.
        If a \code{colormap} is provided, then its properties takes precedence
        over \code{breaks}, \code{col}, \code{missingColor}, and \code{zclip}
        specified to \code{mapImage}.}
  
    \item{border}{colour used for borders of patches (passed to
        \code{\link{polygon}}); the default \code{NA} means no border.}

    \item{lwd}{line width, used if borders are drawn}
    
    \item{lty}{line type, used if borders are drawn}

    \item{missingColor}{a color to be used to indicate missing data, or
        \code{NA} to skip the drawing of such regions (which will retain
        whatever material has already been drawn at the regions).}

    \item{filledContour}{either a boolean value indicating whether to use
        filled contours to plot the image, or a numerical value indicating the
        resampling rate to be used in interpolating from lon-lat coordinates to
        x-y coordinates.  See \dQuote{Details} for how this interacts with
        \code{gridder}.}

    \item{gridder}{name of gridding function used if \code{filledContour} is
        \code{TRUE}.  This can be either \code{"binMean2D"} to select
        \code{\link{binMean2D}} or \code{"interp"} for
        \code{\link[akima]{interp}}. If not provided, then a selection is made
        automatically, with \code{\link{binMean2D}} being used if there are
        more than 10,000 data points in the present graphical view. This
        \code{"binMean2D"} method is much faster than \code{"interp"}.}

    \item{debug}{a flag that turns on debugging.  Set to 1 to get a
        moderate amount of debugging information, or to 2 to get more.
        (Temporary note: setting this to 99 forces the use of a drawing method
        that draws polygons one by one, which is possibly less error prone in
        terms of pruning data, but is much slower than the default method.
        This 99 option will be removed without notice, as it is really to be
        used mainly by the developers and testers.)}
}
    
\details{Adds an image to an existing map, by analogy to \code{\link{image}}.

    The data are on a regular grid in lon-lat space, but not in the projected
    x-y space.  This means that \code{\link{image}} cannot be used.  Instead,
    there are two approaches, depending on the value of \code{filledContour}.

    If \code{filledContour} is \code{FALSE}, the image ``pixels'' are with
    \code{\link{polygon}}, which can be prohibitively slow for fine grids. 

    If \code{filledContour} is \code{TRUE} or a numerical value, then the
    ``pixels'' are remapped into a regular grid and then displayed with
    \code{\link{.filled.contour}}.  The remapping starts by converting the
    regular lon-lat grid to an irregular x-y grid using
    \code{\link{lonlat2map}}.  This irregular grid is then interpolated onto a
    regular x-y grid  with \code{\link{binMean2D}} or with
    \code{\link[akima]{interp}} from the \code{akima} package, as determined by
    the \code{gridder} argument.   If \code{filledContour} is \code{TRUE}, the
    dimensions of the regular x-y grid is the same as that of the original
    lon-lat grid; otherwise, the number of rows and columns are multiplied by
    the numerical value of \code{filledContour}, e.g. the value 2 means to make
    the grid twice as fine.
    
    Filling contours can produce aesthetically-pleasing results, but the method
    involves interpolation, so the data are not represented exactly and
    analysts are advised to compare the results from the two methods (and
    perhaps various grid refinement values) to guard against misinterpretation.

    If a \code{\link{png}} device is to be used, it is advised to supply
    arguments \code{type="cairo"} and \code{antialias="none"}; see [1].

}

\seealso{See \code{\link{mapPlot}} for general information on plotting maps,
    including other functions.}

\references{1. \url{http://codedocean.wordpress.com/2014/02/03/anti-aliasing-and-image-plots/}}

\examples{
\dontrun{
library(oce)
data(coastlineWorld)
data(topoWorld)

par(mfrow=c(2,1), mar=c(2, 2, 1, 1))
lonlim <- c(-70,-50)
latlim <- c(40,50)
topo <- decimate(topoWorld, by=2) # coarse to illustrate filled contours
topo <- subset(topo, latlim[1] < latitude & latitude < latlim[2])
topo <- subset(topo, (360+lonlim[1]) < longitude & longitude < (360+lonlim[2]))
mapPlot(coastlineWorld, type='l',
        longitudelim=lonlim, latitudelim=latlim,
        proj="polyconic", orientation=c(90,-60,0), grid=TRUE)
breaks <- seq(-5000, 1000, 500)
mapImage(topo, col=oce.colorsGebco, breaks=breaks)
mapLines(coastlineWorld)
box()
mapPlot(coastlineWorld, type='l',
        longitudelim=lonlim, latitudelim=latlim,
        proj="polyconic", orientation=c(90,-60,0), grid=TRUE)
mapImage(topo, filledContour=TRUE, col=oce.colorsGebco, breaks=breaks)
box()
mapLines(coastlineWorld)

## Northern polar region, with colour-coded bathymetry
drawPalette(c(-5000,0), zlim=c(-5000, 0), col=oce.colorsJet)
mapPlot(coastlineWorld, type='l',
        longitudelim=c(-180,180), latitudelim=c(60,120),
        proj="stereographic", grid=TRUE)
mapImage(topoWorld, zlim=c(-5000, 0), col=oce.colorsJet)
mapLines(coastlineWorld[['longitude']], coastlineWorld[['latitude']])

# Levitus SST (requires dataset from http://www.esrl.noaa.gov)
par(mfrow=c(1,1))
library(ncdf4)
con <- nc_open("/data/oar/levitus/temperature_annual_5deg.nc")
lon <- ncvar_get(con, "lon")
lat <- ncvar_get(con, "lat")
SST <- ncvar_get(con, "t_mn")[,,1]
par(mar=rep(1, 4))
Tlim <- c(-2, 30)
drawPalette(Tlim, col=oce.colorsJet)
mapPlot(coastlineWorld, projection="+proj=moll", grid=FALSE)
mapImage(lon, lat, SST, col=oce.colorsJet, zlim=Tlim)
mapPolygon(coastlineWorld, col='gray')
}
}

\author{Dan Kelley}

\keyword{hplot}
