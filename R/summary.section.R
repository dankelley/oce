summary.section <- function(object, quiet=FALSE, ...)
{
	if (!inherits(object, "section")) stop("method is only for section objects")
	num.stations <- length(object$data$station)
	if (!quiet) cat("Section", object$metadata$section.id, "has stations:\n")
	have.water.depth <- !is.na(object$data$station[[1]]$water.depth)
	lat1 <- object$metadata$latitude[1]
	lon1 <- object$metadata$longitude[1]
	depth <- vector("numeric", num.stations)
	distance <- vector("numeric", num.stations)
	levels <- vector("numeric", num.stations)
	station.id <- object$metadata$station.id
   	lat.fmt  <- lat.format(object$metadata$latitude)
   	lon.fmt  <- lon.format(object$metadata$longitude)
	for (i in 1:num.stations) {
		stn <- object$data$station[[i]]
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
	invisible(data.frame(station=station.id, latitude=object$metadata$latitude, longitude=object$metadata$longitude))
}
