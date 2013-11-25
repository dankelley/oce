\name{mapImage}

\alias{mapImage}

\title{Plot an image on a existing map}

\description{Plot an image on an existing map}

\usage{mapImage(longitude, latitude, z, zlim, zclip=FALSE, breaks,
         col, border=NA,
         lwd=par("lwd"), lty=par("lty"),
         filledContour=FALSE, missingColor=NA, debug=getOption("oceDebug"))}

\arguments{

    \item{longitude}{longitudes of grid lines}

    \item{latitude}{latitudes of grid lines}

    \item{z}{matrix for image}
    
    \item{zlim}{limit for z (colour)}
    
    \item{zclip}{logical, indicating whether to clip the colours to those
        corresponding to \code{zlim}, if the latter is provided.  Clipped
        regions will be coloured with \code{missingColor}.}

    \item{breaks}{the z values for breaks in the colour scheme.  If this is of
        length 1, the value indicates the desired number of breaks, which is
        supplied to \code{\link{pretty}}, in determining clean break points.}

    \item{col}{either a vector of colours corresponding to the breaks, of length
        1 plus the number of breaks, or a function specifying colours,
        e.g. \code{\link{oceColorsJet}} for a rainbow.}
  
    \item{border}{colour used for borders of patches (passed to
        \code{\link{polygon}}); the default \code{NA} means no border.}

    \item{lwd}{line width, used if borders are drawn}
    
    \item{lty}{line type, used if borders are drawn}

    \item{filledContour}{boolean value indicating whether to use filled
        contours to plot the image.  This is ignored at present.}

    \item{missingColor}{a color to be used to indicate missing data, or
        \code{NA} to skip the drawing of such regions (which will retain
        whatever material has already been drawn at the regions).}

    \item{debug}{a flag that turns on debugging.  Set to 1 to get a
        moderate amount of debugging information, or to 2 to get more.
        (Temporary note: setting this to 99 forces the use of a drawing method
        that draws polygons one by one, which is possibly less error prone in
        terms of pruning data, but is much slower than the default method.
        This 99 option will be removed without notice, as it is really to be
        used mainly by the developers and testers.}
    
\details{Adds an image to an existing map, by analogy to \code{\link{image}}.}

\seealso{See \code{\link{mapPlot}} for general information on plotting maps,
    including other functions.}

\examples{
\dontrun{
library(oce)
data(coastlineWorld)
mapPlot(coastlineWorld, type='l',
        longitudelim=c(-70,-50), latitudelim=c(40,50),
        proj="polyconic", orientation=c(90, -60,0), grid=TRUE)
data(topoWorld)
mapImage(topoWorld, col=oceColorsGebco)
mapMeridians(10, lty='dotted', col='darkgray')
mapZones(10, lty='dotted', col='darkgray')
mapLines(coastlineWorld)

## Northern polar region, with colour-coded bathymetry
drawPalette(c(-5000,0), zlim=c(-5000, 0), col=oceColorsJet)
mapPlot(coastlineWorld, type='l',
         longitudelim=c(-180,180), latitudelim=c(60,120),
         proj="stereographic", grid=TRUE)
data(topoWorld)
mapImage(topoWorld, zlim=c(-5000, 0), col=oceColorsJet)
mapMeridians()
mapZones()
mapLines(coastlineWorld[['longitude']], coastlineWorld[['latitude']])

# Levitus SST (requires dataset from http://www.esrl.noaa.gov)
library(ncdf)
con <- open.ncdf("/data/oar/levitus/temperature_annual_1deg.nc")
#con <- open.ncdf("/data/oar/levitus/temperature_annual_5deg.nc")
lon <- get.var.ncdf(con, "lon")
lat <- get.var.ncdf(con, "lat")
SST <- get.var.ncdf(con, "t_an")[,,1]
#SST <- get.var.ncdf(con, "t_mn")[,,1]
par(mar=rep(1, 4))
Tlim <- c(-2, 30)
drawPalette(Tlim, col=oceColorsJet)
mapPlot(coastlineWorld, projection='mollweide', grid=FALSE)
mapImage(lon, lat, SST, col=oceColorsJet, zlim=Tlim, debug=99)
mapPolygon(coastlineWorld, col='gray')
}
}

\author{Dan Kelley}
\keyword{hplot}
