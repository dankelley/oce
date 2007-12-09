plot.section <- function (x, field=NULL, at=NULL, labels=TRUE,
	grid = FALSE,
	col.grid = "lightgray", 
	station.indices,
	coastline=NULL,
	map.xlim=NULL,
	...)
{
	plot.subsection <- function(variable="temperature", title="Temperature", ...)
	{
		if (variable == "map") {
			lat <- array(NA, num.stations)
			lon <- array(NA, num.stations)
			for (i in 1:num.stations) {
				lat[i] <- x$stations[[station.indices[i]]]$latitude
				lon[i] <- x$stations[[station.indices[i]]]$longitude
			}
			asp <- 1 / cos(mean(range(lat,na.rm=TRUE))*pi/180)
			if (!is.null(map.xlim))
				plot(lon, lat, xlim=map.xlim, asp=asp, type='n')
			else
				plot(lon, lat, asp=asp, type='n')
			if (!is.null(coastline)) {
				if (mean(lon, na.rm=TRUE) > 0)
					lines(coastline$data$longitude, coastline$data$latitude, col="darkgray")
				else 
					lines(coastline$data$longitude, coastline$data$latitude, col="darkgray")
			}
			lines(lon, lat)
			points(lon, lat, pch=20)
			points(lon[1], lat[1], pch=22, cex=2*par("cex"))
		} else {
			if (is.null(at)) {
				plot(range(xx), range(yy),
		 			yaxs="i", ylim=rev(range(yy)), col="white", 
					xlab="Distance [ km ]",	ylab="Pressure [ dbar ]")
			} else {
				plot(range(xx), range(yy),
		 		yaxs="i", ylim=rev(range(yy)), col="white", 
				xlab="", ylab="Pressure [ dbar ]", axes=FALSE)
				axis(1, at=at, labels=labels)
				axis(2)
				box()
			}
			for (i in 1:num.stations) {
				zz[i,] <- rev(x$stations[[station.indices[i]]]$data[[variable]])
				if (grid) {
					abline(v = xx[i], col=col.grid, lty="dotted")
				}
			}
			par(new=TRUE)
			contour(x=xx, y=yy, z=zz, axes=FALSE, ...)
			legend("topright", title, bg="white", x.intersp=0, y.intersp=0.5)
		}
	}
	
	if (!inherits(x, "section")) stop("method is only for section objects")
	oldpar <- par(no.readonly = TRUE)

	if (missing(station.indices)) {
		num.stations <- length(x$stations)
		station.indices <- 1:num.stations
	} else {
		num.stations <- length(station.indices)		
	}
	if (num.stations < 2) stop("cannot plot a section containing less than 2 stations")
	num.depths <- length(x$stations[[station.indices[1]]]$data$pressure)
	zz <- matrix(nrow=num.stations, ncol=num.depths)
	xx <- array(NA, num.stations)
	yy <- array(NA, num.depths)
	if (is.null(at)) {
		lat0 <- x$stations[[station.indices[1]]]$latitude
		lon0 <- x$stations[[station.indices[1]]]$longitude
		for (ix in 1:num.stations) {
			j <- station.indices[ix]
			xx[ix] <- geod.dist(lat0, lon0,x$stations[[j]]$latitude, x$stations[[j]]$longitude)
		}
	} else {
		xx <- at
	}
	yy <- x$stations[[station.indices[1]]]$data$pressure
	if (is.null(field)) {
		par(mfrow=c(2,2))
		par(mar=c(4.5,4,1,1))
		plot.subsection("temperature", "T", ...)
		plot.subsection("salinity",    "S", ...)
		plot.subsection("sigma.theta",  expression(sigma[theta]), ...)
		plot.subsection("map")
	} else {
		field.name <- field
		if (field == "sigma.theta") field.name <- expression(sigma[theta])
		plot.subsection(field, field.name, ...)
	}
	par(oldpar)
}
