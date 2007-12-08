plot.section <- function (x, field=NULL, at=NULL, labels=TRUE,
	grid = FALSE, 
	col.grid = "lightgray", 
	coastline = NULL,
	levels = NA,
	...)
{
	plot.subsection <- function(variable="temperature", title="Temperature", levels=NA, ...)
	{
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
			zz[i,] <- rev(x$stations[[i]]$data[[variable]])
			if (grid) {
				abline(v = xx[i], col=col.grid, lty="dotted")
			}
		}
		par(new=TRUE)
		if (length(levels) == 1 && is.na(levels))
			contour(x=xx, y=yy, z=zz, axes=FALSE, ...)
		else
			contour(x=xx, y=yy, z=zz, axes=FALSE, levels, ...)
		legend("topright", title, bg="white", x.intersp=0, y.intersp=0.5)
	}
	
    if (!inherits(x, "section")) 
        stop("method is only for section objects")
    oldpar <- par(no.readonly = TRUE)
	num.stations <- length(x$stations)
	if (num.stations < 2)
		stop("cannot plot a section containing less than 2 stations")
	num.depths <- length(x$stations[[1]]$data$pressure)
	#cat("num.stations", num.stations, " num.depths",num.depths, "\n")
	zz <- matrix(nrow=num.stations, ncol=num.depths)
	xx <- array(NA, num.stations)
	if (is.null(at)) {
		for (ix in 1:num.stations) {
			xx[ix] <- geod.dist(x$stations[[1]]$latitude, x$stations[[1]]$longitude,
				x$stations[[ix]]$latitude, x$stations[[ix]]$longitude)
		}
	} else {
		xx <- at
	}
	yy <- x$stations[[1]]$data$pressure
	if (is.null(field)) {
		par(mfrow=c(2,2))
		par(mar=c(4.5,4,1,1))
		plot.subsection("temperature", "T")
		plot.subsection("salinity",    "S")
		plot.subsection("sigma.theta",  expression(sigma[theta]))
		ss <- summary(x, quiet=TRUE)
		if (!is.null(coastline)) {
			plot.coastline(coastline, col="darkgray")
		} else {
			asp <- 1 / cos(mean(range(ss$latitude,na.rm=TRUE))*pi/180)
		    plot(ss$longitude, ss$latitude, asp=asp, type="p", xlab="", ylab="")
		}
		lines(ss$longitude, ss$latitude, ...)
		points(ss$longitude, ss$latitude, pch=20, ...)
		points(ss$longitude[1], ss$latitude[1], pch=22, cex=2*par("cex"), ...)
		#text(ss$longitude[1], ss$latitude[1], "x=0")
	} else {
		plot.subsection(field, field, levels, ...)
	}
	par(oldpar)
}
