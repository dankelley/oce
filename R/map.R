#source('~/src/R-kelley/oce/R/map.R'); mapPlot(lon, lat, pro="orthographic",type='l',xlim=c(-80,10),ylim=c(0,120), orientation=c(45,-100,0))

library(mapproj)

##mapContour <- function()
##{
##    stop("mapContour does not do anything yet")
##}
##

mapPlot <- function(longitude, latitude, longitudelim, latitudelim, grid,
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
    usr <- par('usr')
    if (!missing(grid)) {
        if (is.logical(grid))
            grid <- 10
        lons <- rep(seq(-180, 180, grid), each=180/grid)
        lats <- rep(seq(-90, 90, grid), each=360/grid)
        n <- 360 # points on line
        for (lon in seq(-180, 180, grid)) {
            line <- mapproject(rep(lon, n), seq(-90+grid, 90-grid, length.out=n))
            ok <- !is.na(line$x) & !is.na(line$y)
            if (any(usr[1] <= line$x[ok] & line$x[ok] <= usr[2] & usr[3] <= line$y[ok] & line$y[ok] <= usr[4])) {
                lines(line$x, line$y, col='gray', lty='dotted')
            }
        }
        for (lat in seq(-90, 90-grid, grid)) {
            line <- mapproject(seq(-180, 180, length.out=n), rep(lat, n))
            ok <- !is.na(line$x) & !is.na(line$y)
            if (any(usr[1] <= line$x[ok] & line$x[ok] <= usr[2] & usr[3] <= line$y[ok] & line$y[ok] <= usr[4])) {
                lines(line$x, line$y, col='gray', lty='dotted')
            }
        }
    }

    box()
    ## FIXME: add lat-lon grid
}

mapLines <- function(longitude, latitude, ...)
{
    xy <- mapproject(longitude, latitude)
    lines(xy$x, xy$y, ...)
}

mapPoints <- function(longitude, latitude, ...)
{
    xy <- mapproject(longitude, latitude)
    points(xy$x, xy$y, ...)
}

mapText <- function(longitude, latitude, labels, ...)
{
    xy <- mapproject(longitude, latitude)
    text(xy$x, xy$y, labels, ...)
}

