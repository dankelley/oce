plot.section <- function (x, field=NULL, at=NULL, labels=TRUE,
	grid = FALSE,
	station.indices,
	coastline=NULL,
	map.xlim=NULL,
	...)
{
	plot.subsection <- function(variable="temperature", title="Temperature", indicate.stations=TRUE, ...)
	{
		if (variable == "map") {
			lat <- array(NA, num.stations)
			lon <- array(NA, num.stations)
			for (i in 1:num.stations) {
				lat[i] <- x$station[[station.indices[i]]]$latitude
				lon[i] <- x$station[[station.indices[i]]]$longitude
			}
			asp <- 1 / cos(mean(range(lat,na.rm=TRUE))*pi/180)
			if (!is.null(map.xlim))
				plot(lon, lat, xlim=map.xlim, asp=asp, type='n', xlab="Longitude", ylab="Latitude")
			else
				plot(lon, lat, asp=asp, type='n', xlab="Longitude", ylab="Latitude")
			if (!is.null(coastline)) {
				if (mean(lon, na.rm=TRUE) > 0)
					lines(coastline$data$longitude, coastline$data$latitude, col="darkgray")
				else 
					lines(coastline$data$longitude, coastline$data$latitude, col="darkgray")
			}
			lines(lon, lat)
			points(lon, lat, pch=20)
			points(lon[1], lat[1], pch=22, cex=2*par("cex"))
			if (indicate.stations) {
				dx <- 5 * mean(diff(sort(x$longitude)),na.rm=TRUE)
				dy <- 5 * mean(diff(sort(x$latitude)),na.rm=TRUE)
				xlab <- x$longitude[1] - dx * sign(x$longitude[2] - x$longitude[1])
				ylab <- x$latitude[1]  - dy * sign(x$latitude[2]  - x$latitude[1])
				text(xlab, ylab, x$station.id[1])
				xlab <- x$longitude[num.stations] - dx * sign(x$longitude[num.stations-1] - x$longitude[num.stations])
				ylab <- x$latitude[num.stations]  - dy * sign(x$latitude[num.stations-1]  - x$latitude[num.stations])
				text(xlab, ylab, x$station.id[num.stations])
			}
		} else {
			# FIXME: contours don't get to plot edges with axs="i"
			if (is.null(at)) {
				plot(range(xx), range(yy),
		 			xaxs="i", yaxs="r", ylim=rev(range(yy)), col="white", 
					xlab="Distance [ km ]",	ylab="Pressure [ dbar ]")
			} else {
				plot(range(xx), range(yy),
		 			xaxs="i", yaxs="r", ylim=rev(range(yy)), col="white", 
					xlab="", ylab="Pressure [ dbar ]", axes=FALSE)
				axis(1, at=at, labels=labels)
				axis(2)
				box()
			}
			water.depth <- NULL
			for (i in 1:num.stations) {
				zz[i,] <- rev(x$station[[station.indices[i]]]$data[[variable]])
				if (grid)
					points(rep(xx[i], length(yy)), yy, col="gray", pch=20, cex=0.5)
				water.depth <- c(water.depth, x$station[[station.indices[i]]]$water.depth)
			}
			# draw the ground below the water
			graph.bottom <- par("usr")[3]
			bottom.x <- c(xx[1], xx, xx[length(xx)])
			bottom.y <- c(graph.bottom, water.depth, graph.bottom)
			polygon(bottom.x, bottom.y, col="gray") # bottom trace
			par(new=TRUE)
			dots <- list(...) # adjust plot parameter labcex, unless user did
			if (is.null(dots$labcex)) {
				contour(x=xx, y=yy, z=zz, axes=FALSE, labcex=0.8, ...)
			} else {
				contour(x=xx, y=yy, z=zz, axes=FALSE, ...)
			}
			legend("topright", title, bg="white", x.intersp=0, y.intersp=0.5)
		}
	}
	
	if (!inherits(x, "section")) stop("method is only for section objects")
	oldpar <- par(no.readonly = TRUE)

	if (missing(station.indices)) {
		num.stations <- length(x$station)
		station.indices <- 1:num.stations
	} else {
		num.stations <- length(station.indices)		
	}
	if (num.stations < 2) stop("cannot plot a section containing fewer than 2 stations")
	num.depths <- length(x$station[[station.indices[1]]]$data$pressure)
	zz <- matrix(nrow=num.stations, ncol=num.depths)
	xx <- array(NA, num.stations)
	yy <- array(NA, num.depths)
	if (is.null(at)) {
		lat0 <- x$station[[station.indices[1]]]$latitude
		lon0 <- x$station[[station.indices[1]]]$longitude
		for (ix in 1:num.stations) {
			j <- station.indices[ix]
			xx[ix] <- geod.dist(lat0, lon0,x$station[[j]]$latitude, x$station[[j]]$longitude)
		}
	} else {
		xx <- at
	}
	yy <- x$station[[station.indices[1]]]$data$pressure
	if (is.null(field)) {
		par(mfrow=c(2,2))
		par(mar=c(4.5,4,1,1))
		plot.subsection("temperature", "T", ...)
		plot.subsection("salinity",    "S", ...)
		plot.subsection("sigma.theta",  expression(sigma[theta]), ...)
		plot.subsection("map", indicate.stations=FALSE)
	} else {
		field.name <- field
		if (field == "sigma.theta") field.name <- expression(sigma[theta])
		plot.subsection(field, field.name, ...)
	}
	par(oldpar)
}
