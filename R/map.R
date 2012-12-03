#source('~/src/R-kelley/oce/R/map.R'); mapPlot(lon, lat, pro="orthographic",type='l',xlim=c(-80,10),ylim=c(0,120), orientation=c(45,-100,0))

library(mapproj)

##mapContour <- function()
##{
##    stop("mapContour does not do anything yet")
##}
##
##mapLines <- function()
##{
##    stop("mapLines does not do anything yet")
##}

mapPlot <- function(latitude, longitude, latitudelim, longitudelim,
                    projection="mercator", parameters=NULL, orientation=NULL,
                    ...)
{
    xy <- mapproject(longitude, latitude,
                     projection=projection, parameters=parameters, orientation=orientation)
    limitsGiven <- !missing(latitudelim) && !missing(longitudelim)
    if (limitsGiven) {
        box <- mapproject(c(longitudelim[1], longitudelim[1], longitudelim[2], longitudelim[2]),
                          c(latitudelim[1], latitudelim[2], latitudelim[2], latitudelim[1]))
        plot(xy$x, xy$y,
             xlim=range(box$x, na.rm=TRUE), ylim=range(box$y, na.rm=TRUE),
             xlab="", ylab="", asp=1, axes=FALSE, ...)
    } else {
        plot(xy$x, xy$y,
             xlab="", ylab="", asp=1, axes=FALSE, ...)
    }
    box()
    ## FIXME: add lat-lon grid
}

mapLines <- function(latitude, longitude, ...)
{
    xy <- mapproject(longitude, latitude)
    lines(xy$x, xy$y, ...)
}

mapPoints <- function(latitude, longitude, ...)
{
    xy <- mapproject(longitude, latitude)
    points(xy$x, xy$y, ...)
}

## mapText <- function()
## {
##     stop("mapText does not do anything yet")
## }
## 
