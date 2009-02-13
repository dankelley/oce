plot.ctd <- function (x, ref.lat = NaN, ref.lon = NaN,
                      grid = TRUE, col.grid="lightgray",
                      which = 1:4,
                      coastline,
                      Slim, Tlim, plim, lonlim, latlim,
                      latlon.pch=20, latlon.cex=1.5, latlon.col="red",
                      ...)
{
    dec_deg <- function(x, code = "lat") {
        if (code == "lat") {
            if (x < 0) {
                x <- -x
                sprintf("%.0f %.2fS", floor(x), 60 * (x - floor(x)))
            }
            else {
                sprintf("%.0f %.2fN", floor(x), 60 * (x - floor(x)))
            }
        } else {
            if (x < 0) {
                x <- -x
                sprintf("% %.2fW", floor(x), 60 * (x - floor(x)))
            }
            else {
                sprintf("% %.2fE", floor(x), 60 * (x - floor(x)))
            }
        }
    }
    if (!inherits(x, "ctd")) stop("method is only for ctd objects")
    oldpar <- par(no.readonly = TRUE)
    if (!"mgp" %in% names(list(...))) par(mgp = c(2, 2/3, 0))

    ## 1=S+T
    ## 2=density+N2
    ## 3=TS
    ## 4=text
    ## 5=map
    ## 6=density+dpdt
    par(mfrow = c(2, 2))
    par(mar=c(3,3,3.25,2))

    for (w in 1:length(which)) {
        if (which[w] == 1) plot.profile(x, type = "S+T", grid=grid, col.grid=col.grid, Slim=Slim, Tlim=Tlim, plim=plim, ...)
        if (which[w] == 2) plot.profile(x, type = "density+N2", grid=grid, col.grid=col.grid, plim=plim, ...)
        if (which[w] == 6) plot.profile(x, "density+dpdt", plim=plim, ...)
        if (which[w] == 3) {
            par(mar=c(3.5,3,2,2))
            plot.TS(x, grid=grid, col.grid=col.grid, Slim=Slim, Tlim=Tlim, ...)
        }
        if (which[w] == 4) {
            text.item <- function(item, label, cex=0.8) {
                if (!is.null(item) && !is.na(item)) {
                    text(xloc, yloc, paste(label, item), adj = c(0, 0), cex=cex);
                    yloc <<- yloc - d.yloc;
                }
            }
            xfake <- seq(0:10)
            yfake <- seq(0:10)
            par(mar=c(0,0,0,0))
            plot(xfake, yfake, type = "n", xlab = "", ylab = "", axes = FALSE)
            xloc <- 1
            yloc <- 10
            d.yloc <- 0.7
            cex <- 0.8
            text(xloc, yloc, paste("CTD Station"), adj = c(0, 0), cex=cex)
            yloc <- yloc - d.yloc
            xm <- x$metadata
            if (!is.null(xm$filename))    	text.item(xm$filename,    " File:     ", cex=cex)
            if (!is.null(xm$scientist))	text.item(xm$scientist,   " Scientist:", cex=cex)
            if (!is.null(xm$institute))	text.item(xm$institute,   " Institute:", cex=cex)
            if (!is.null(xm$date))    	text.item(xm$date,        " Date:     ", cex=cex)
            if (!is.null(xm$ship))		text.item(xm$ship,        " Ship:     ", cex=cex)
            if (!is.null(xm$cruise))    	text.item(xm$cruise,      " Cruise:   ", cex=cex)
            if (!is.null(xm$station))    	text.item(xm$station,     " Station:  ", cex=cex)
            if (!is.null(xm$water.depth))  	text.item(xm$water.depth, " Depth:    ", cex=cex)
            if (!is.na(xm$longitude) && !is.na(xm$latitude)) {
                text.item(latlon.format(xm$latitude, xm$longitude),   " Location: ", cex=cex)
            }
            if (!is.na(ref.lat) && !is.na(ref.lon)) {
                dist <- geod.dist(xm$latitude, xm$longitude, ref.lat, ref.lon)
                kms <- sprintf("%.2f km", dist/1000)
                rlat <- text(xloc, yloc, paste(" Distance to (", dec_deg(ref.lon),
                                               ",", dec_deg(ref.lat), ") = ", kms), adj = c(0, 0), cex=cex)
                yloc <- yloc - d.yloc
            }
        }
        if (which[w] == 5) {
            if (missing(coastline)) stop("need a coastline file to draw a map")
            if (!missing(lonlim) && !missing(latlim))
                plot(coastline, xlim=lonlim, ylim=latlim)
            else
                plot(coastline)
            points(x$metadata$longitude, x$metadata$latitude, cex=latlon.cex, col=latlon.col, pch=latlon.pch)
            title(paste("Station", x$metadata$station),font.main=par("font"))
        }
    }
    par(oldpar)
    invisible()
}
