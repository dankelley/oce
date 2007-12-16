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
	station.id <- object$station.id
   	lat.fmt  <- lat.format(object$latitude)
   	lon.fmt  <- lon.format(object$longitude)
	for (i in 1:num.stations) {
		stn <- object$station[[i]]
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
