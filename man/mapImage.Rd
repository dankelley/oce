\name{mapImage}

\alias{mapImage}

\title{Plot an image on a existing map}

\description{Plot an image on an existing map}

\usage{mapImage(longitude, latitude, z, zlim, zclip=FALSE,
         breaks, col, colormap, border=NA,
         lwd=par("lwd"), lty=par("lty"),
         filledContour=FALSE, missingColor=NA, debug=getOption("oceDebug"))}

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
        e.g. \code{\link{oceColorsJet}} for a rainbow.}

    \item{colormap}{optional colormap, as created by \code{\link{colormap}}.
        If provided, this takes precedence over \code{breaks}, code{col} and
        \code{missingColor}.}
  
    \item{border}{colour used for borders of patches (passed to
        \code{\link{polygon}}); the default \code{NA} means no border.}

    \item{lwd}{line width, used if borders are drawn}
    
    \item{lty}{line type, used if borders are drawn}

    \item{filledContour}{either boolean value indicating whether to use filled
        contours to plot the image, or numerical value indicating the
        resampling rate to be used in interpolating from lon-lat coordinates to
        x-y coordinates.  See \dQuote{Details}.}

    \item{missingColor}{a color to be used to indicate missing data, or
        \code{NA} to skip the drawing of such regions (which will retain
        whatever material has already been drawn at the regions).}

    \item{debug}{a flag that turns on debugging.  Set to 1 to get a
        moderate amount of debugging information, or to 2 to get more.
        (Temporary note: setting this to 99 forces the use of a drawing method
        that draws polygons one by one, which is possibly less error prone in
        terms of pruning data, but is much slower than the default method.
        This 99 option will be removed without notice, as it is really to be
        used mainly by the developers and testers.)}
}
    
\details{Adds an image to an existing map, by analogy to \code{\link{image}}.

    For the case where \code{filledContour} is \code{FALSE}, the results can
    seem ``washed out'' on some plotting devices, owing to an issue with
    anti-aliasing in the drawing of the pixel polygons.  If a \code{\link{png}}
    device is to be used, it is advised to supply arguments \code{type="cairo"}
    and \code{antialias="none"}; see [1].

    For the case where \code{filledContour} is \code{TRUE} or a numerical
    value, \code{\link{.filled.contour}} is used as follows.  First, the
    lon-lat grid, which is regular, is converted to an intermediate irregular
    x-y grid using lon-lat box to x-y space using \code{\link{lonlat2map}}.
    Then, the latter is converted to a regular x-y grid  with
    \code{\link[akima]{interp}} from the \code{akima} package.  This regular
    x-y grid is indicated with filled contours.  If \code{filledContour} is
    \code{TRUE}, the dimension of the regular x-y grid is the same as that of
    the lon-lat grid; otherwise, the number of rows will be multiplied by the
    numerical value of \code{filledContour}.
    
    The advantage of filling contours is that it can look less pixelated for a
    coarse grid, but the disadvantage is that some interpolation is done, so
    the data are not represented exactly.

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
topo <- decimate(topoWorld, by=3) # coarse to illustrate filled contours
mapPlot(coastlineWorld, type='l',
        longitudelim=c(-70,-50), latitudelim=c(40,50),
        proj="polyconic", orientation=c(90, -60,0), grid=TRUE)
mapImage(topo, col=oceColorsGebco, breaks=seq(-5000, 0000, 100))
mapLines(coastlineWorld)
box()
mapPlot(coastlineWorld, type='l',
        longitudelim=c(-70,-50), latitudelim=c(40,50),
        proj="polyconic", orientation=c(90, -60,0), grid=TRUE)
mapImage(topo, filledContour=TRUE, col=oceColorsJet)
box()
mapLines(coastlineWorld)

## Northern polar region, with colour-coded bathymetry
drawPalette(c(-5000,0), zlim=c(-5000, 0), col=oceColorsJet)
mapPlot(coastlineWorld, type='l',
        longitudelim=c(-180,180), latitudelim=c(60,120),
        proj="stereographic", grid=TRUE)
mapImage(topoWorld, zlim=c(-5000, 0), col=oceColorsJet)
mapLines(coastlineWorld[['longitude']], coastlineWorld[['latitude']])

# Levitus SST (requires dataset from http://www.esrl.noaa.gov)
library(ncdf4)
con <- nc_open("/data/oar/levitus/temperature_annual_5deg.nc")
lon <- ncvar_get(con, "lon")
lat <- ncvar_get(con, "lat")
SST <- ncvar_get(con, "t_mn")[,,1]
par(mar=rep(1, 4))
Tlim <- c(-2, 30)
drawPalette(Tlim, col=oceColorsJet)
mapPlot(coastlineWorld, projection='mollweide', grid=FALSE)
mapImage(lon, lat, SST, col=oceColorsJet, zlim=Tlim)
mapPolygon(coastlineWorld, col='gray')
}
}

\author{Dan Kelley}
\keyword{hplot}
