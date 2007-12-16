summary.section <- function(object, quiet=FALSE, ...)
{
	if (!inherits(object, "section")) stop("method is only for section objects")
	num.stations <- length(object$station)
	if (!quiet) cat("Section", object$section.id, "has stations:\n")
	have.water.depth <- !is.na(object$station[[1]]$water.depth)
	lat1 <- object$latitude[1]
	lon1 <- object$longitude[1]
	depth <- vector("numeric", num.stations)
	distance <- vector("numeric", num.stations)
	levels <- vector("numeric", num.stations)
	station.id <- vector("character", num.stations)
	lat.fmt <- vector("character", num.stations)
	lon.fmt <- vector("character", num.stations)
	for (i in 1:num.stations) {
		stn <- object$station[[i]]
		station.id[i]  <- object$station.id[i]
    	lat.fmt[i]  <- lat.format(object$latitude[i])
    	lon.fmt[i]  <- lon.format(object$longitude[i])
		depth[i] <- if (have.water.depth) stn$water.depth else max(stn$data$pressure, na.rm=TRUE)
		distance[i] <- sprintf("%.1fkm", geod.dist(lat1, lon1, stn$latitude, stn$longitude))
		levels[i] <- length(object$station[[i]]$data$pressure)
	}
	if (!quiet) {
		if (have.water.depth) {
			print(data.frame(Station=station.id, Latitude=lat.fmt, Longitude=lon.fmt, Depth=depth, Distance=distance, Levels=levels))
		} else {
			print(data.frame(Station=station.id, Latitude=lat.fmt, Longitude=lon.fmt, Distance=distance, Levels=levels))
		}
		processing.log.summary(object)
	}
	invisible(data.frame(station=station.id, latitude=object$latitude, longitude=object$longitude))
}
