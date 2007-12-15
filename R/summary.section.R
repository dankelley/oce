summary.section <- function(object, quiet=FALSE, ...)
{
	if (!inherits(object, "section")) stop("method is only for section objects")
	num.stations <- length(object$station)
	if (!quiet) cat("Section", object$section.id, "has stations:\n")
	have.water.depth <- !is.na(object$station[[1]]$water.depth)
	filename <- station <-lat <- lat.fmt <- lon <- lon.fmt <- distance <- levels <- NULL		
	if (have.water.depth)
		depth <- NULL
	lat.1 <- object$station[[1]]$latitude
	lon.1 <- object$station[[1]]$longitude
	for (i in 1:num.stations) {
		stn <- object$station[[i]]
		station  <- c(station,  stn$station)
		filename <- c(filename, stn$filename)
    	lat      <- c(lat,      stn$latitude)
    	lon      <- c(lon,      stn$longitude)
    	lat.fmt  <- c(lat.fmt,  lat.format(stn$latitude))
    	lon.fmt  <- c(lon.fmt,  lon.format(stn$longitude))
		if (have.water.depth)
			depth    <- c(depth,   stn$water.depth)
		distance <- c(distance, sprintf("%.1f km", geod.dist(lat.1, lon.1, stn$latitude, stn$longitude)))
		levels   <- c(levels,  length(stn$data$pressure))
	}
	if (!quiet) {
		if (have.water.depth) {
			print(data.frame(Filename=filename, Station=station, Latitude=lat.fmt, Longitude=lon.fmt, Water.Depth=depth, Distance=distance, Levels=levels))
		} else {
			print(data.frame(Filename=filename, Station=station, Latitude=lat.fmt, Longitude=lon.fmt, Distance=distance, Levels=levels))
		}
		processing.log.summary(object)
	}
	invisible(data.frame(filename=filename, station=station, latitude=lat, longitude=lon))
}
