plot.section <- function (x, field=NULL, at=NULL, labels=TRUE,
                          grid = FALSE,
                          contour.levels=NULL,
                          contour.labels=NULL,
                          station.indices,
                          coastline=NULL,
                          map.xlim=NULL,
                          xtype="distance",
                          ...)
{
    plot.subsection <- function(variable="temperature", title="Temperature", indicate.stations=TRUE, contour.levels=NULL, contour.labels=NULL, xtype=1, ...)
    {
        if (variable == "map") {
            lat <- array(NA, num.stations)
            lon <- array(NA, num.stations)
            for (i in 1:num.stations) {
                lat[i] <- x$data$station[[station.indices[i]]]$metadata$latitude
                lon[i] <- x$data$station[[station.indices[i]]]$metadata$longitude
            }
            asp <- 1 / cos(mean(range(lat,na.rm=TRUE))*pi/180)
            latm <- mean(lat, na.rm=TRUE)
            lonm <- mean(lon, na.rm=TRUE)
            ## expand the range by 20%
            lonr <- lonm + 1.2 * (range(lon, na.rm=TRUE) - mean(lon, na.rm=TRUE))
            latr <- latm + 1.2 * (range(lat, na.rm=TRUE) - mean(lat, na.rm=TRUE))
            if (!is.null(map.xlim))
                plot(lonr, latr, xlim=map.xlim, asp=asp, type='n', xlab="Longitude", ylab="Latitude")
            else
                plot(lonr, latr, asp=asp, type='n', xlab="Longitude", ylab="Latitude")
            if (!is.null(coastline)) {
                if (mean(lon, na.rm=TRUE) > 0)
                    lines(coastline$data$longitude, coastline$data$latitude, col="darkgray")
                else
                    lines(coastline$data$longitude, coastline$data$latitude, col="darkgray")
            }
            lines(lon, lat, col="lightgray")
            points(lon, lat, pch=20)
            points(lon[1], lat[1], pch=22, cex=2*par("cex"))
            if (indicate.stations) {
                dx <- 5 * mean(diff(sort(x$metadata$longitude)),na.rm=TRUE)
                dy <- 5 * mean(diff(sort(x$metadata$latitude)),na.rm=TRUE)
                xlab <- x$metadata$longitude[1] - dx * sign(x$metadata$longitude[2] - x$metadata$longitude[1])
                ylab <- x$metadata$latitude[1]  - dy * sign(x$metadata$latitude[2]  - x$metadata$latitude[1])
                text(xlab, ylab, x$metadata$station.id[1])
                xlab <- x$metadata$longitude[num.stations] - dx * sign(x$metadata$longitude[num.stations-1] - x$metadata$longitude[num.stations])
                ylab <- x$metadata$latitude[num.stations]  - dy * sign(x$metadata$latitude[num.stations-1]  - x$metadata$latitude[num.stations])
                text(xlab, ylab, x$metadata$station.id[num.stations])
            }
        } else {                        # not a map
            if (!(variable %in% names(x$data$station[[1]]$data))) stop("this section does not contain a variable named '", variable, "'")

            ## FIXME: contours don't get to plot edges
            xxrange <- range(xx)
            yyrange <- range(yy)
            ##yyrange[1] <- -1

            ## Put x in order, if it's not already
            ox <- order(xx)
            if (any(xx[ox] != xx)) {
                xx <- xx[ox]
                zz <- zz[ox,]
                cat("NOTE: plot.section() reordered the stations to make x monotonic\n")
            }

            ylab <- if ("ylab" %in% names(list(...))) list(...)$ylab else "Pressure [ dbar ]"

            if (is.null(at)) {
                plot(xxrange, yyrange,
                     xaxs="i", yaxs="i", ylim=rev(yyrange), col="white",
                     xlab=if (which.xtype==1) "Distance [ km ]" else "Alongtrack Distance [km]",
                     ylab=ylab)
                axis(4, labels=FALSE)
            } else {
                plot(xxrange, yyrange,
                     xaxs="i", yaxs="i", ylim=rev(yyrange), col="white",
                     xlab="", ylab=ylab, axes=FALSE)
                axis(1, at=at, labels=labels)
                axis(2)
                axis(4, labels=FALSE)
                box()
            }
            water.depth <- NULL
            for (i in 1:num.stations) {
                zz[i,] <- rev(x$data$station[[station.indices[i]]]$data[[variable]])
                if (grid)
                    points(rep(xx[i], length(yy)), yy, col="gray", pch=20, cex=1/3)
                else
                    Axis(side=3, at=xx, labels=FALSE, lwd=0.5) # station locations
                water.depth <- c(water.depth,
                                 max(x$data$station[[station.indices[i]]]$data$depth, na.rm=TRUE))
            }
                                        # draw the ground below the water
            graph.bottom <- par("usr")[3]
            bottom.x <- c(xx[1], xx, xx[length(xx)])
            bottom.y <- c(graph.bottom, water.depth, graph.bottom)
            polygon(bottom.x, bottom.y, col="gray") # bottom trace
            par(new=TRUE)
            dots <- list(...) # adjust plot parameter labcex, unless user did
            if (!is.null(contour.levels) && !is.null(contour.labels)) {
                if (is.null(dots$labcex)) {
                    contour(x=xx, y=yy, z=zz, axes=FALSE, labcex=0.8,
                            levels=contour.levels,
                            labels=contour.labels,
                            ...)
                } else {
                    contour(x=xx, y=yy, z=zz, axes=FALSE, ...)
                }
            } else {
                if (is.null(dots$labcex)) {
                    contour(x=xx, y=yy, z=zz, axes=FALSE, labcex=0.8, ...)
                } else {
                    contour(x=xx, y=yy, z=zz, axes=FALSE, ...)
                }
            }
            legend("topright", title, bg="white", x.intersp=0, y.intersp=0.5)
        }
    }                                   # plot.subsection

    which.xtype = pmatch(xtype, c("distance", "track"), nomatch=0)

    if (!inherits(x, "section")) stop("method is only for section objects")
    oldpar <- par(no.readonly = TRUE)
    if (!"mgp" %in% names(list(...))) par(mgp = c(2, 2/3, 0))
    if (missing(station.indices)) {
        num.stations <- length(x$data$station)
        station.indices <- 1:num.stations
    } else {
        num.stations <- length(station.indices)
    }
    if (num.stations < 2) stop("cannot plot a section containing fewer than 2 stations")
    num.depths <- length(x$data$station[[station.indices[1]]]$data$pressure)

    ## Check that pressures coincide
    p1 <- x$data$station[[station.indices[1]]]$data$pressure
    for (ix in 2:num.stations) {
        if (any(p1 != x$data$station[[station.indices[ix]]]$data$pressure)) {
            stop("This section has stations with different pressure levels.\n  Please use e.g.\n\tsection.gridded <- section.grid(section)\n  to create a uniform grid, and then you'll be able to plot the section.")
        }
    }

    zz <- matrix(nrow=num.stations, ncol=num.depths)
    xx <- array(NA, num.stations)
    yy <- array(NA, num.depths)
    if (is.null(at)) {
        lat0 <- x$data$station[[station.indices[1]]]$metadata$latitude
        lon0 <- x$data$station[[station.indices[1]]]$metadata$longitude
        for (ix in 1:num.stations) {
            j <- station.indices[ix]
            if (which.xtype == 1) {
                xx[ix] <- geod.dist(lat0,
                                    lon0,
                                    x$data$station[[j]]$metadata$latitude,
                                    x$data$station[[j]]$metadata$longitude)
            } else if (which.xtype == 2) {
                if (ix == 1)
                    xx[ix] <- 0
                else
                    xx[ix] <- xx[ix-1] +
                        geod.dist(x$data$station[[station.indices[ix-1]]]$metadata$latitude,
                                  x$data$station[[station.indices[ix-1]]]$metadata$longitude,
                                  x$data$station[[j]]$metadata$latitude,
                                  x$data$station[[j]]$metadata$longitude)
            } else {
                stop("unknown xtype")
            }
        }
    } else {
        xx <- at
    }
    yy <- x$data$station[[station.indices[1]]]$data$pressure
    if (is.null(field)) {
        par(mfrow=c(2,2))
        if (!"mgp" %in% names(list(...))) par(mar = c(3.0, 3.0, 1, 1))
        else par(mar=c(4.5,4,1,1))
        plot.subsection("temperature", "T", ...)
        plot.subsection("salinity",    "S", ylab="",...)
        plot.subsection("sigma.theta",  expression(sigma[theta]), ...)
        plot.subsection("map", indicate.stations=FALSE)
    } else {
        field.name <- field
        if (field == "sigma.theta") field.name <- expression(sigma[theta])
        if (!missing(contour.levels) && !missing(contour.labels)) {
            plot.subsection(field, field.name,
                            contour.levels=contour.levels,
                            contour.labels=contour.labels,
                            ...)
        } else {
            plot.subsection(field, field.name, ...)
        }
    }
#    par(oldpar)
}
