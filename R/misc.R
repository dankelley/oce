latlon.format <- function(lat, lon) {
	n <- length(lon)
	rval <- vector("character", n)
	for (i in 1:n) {
		if (is.na(lat[i]) || is.na(lon[i]))
			rval[i] <- ""
		else
			rval[i] <- sprintf("%8.4f%1s %8.4f%1s",
				abs(lat[i]), if (lat[i] > 0) "N" else "S",
				abs(lon[i]), if (lon[i] > 0) "E" else "W")
	}
	rval
}
lat.format <- function(lat) {
	n <- length(lat)
	rval <- vector("character", n)
	for (i in 1:n)
		rval[i] <- if (is.na(lat[i])) ""
			else sprintf("%8.4f%1s", abs(lat[i]), if (is.na(lat[i]) || lat[i] > 0) "N" else "S")
	rval
}
lon.format <- function(lon) {
	n <- length(lon)
	rval <- vector("character", n)
	for (i in 1:n)
		rval[i] <- if (is.na(lon[i])) ""
			else sprintf("%8.4f%1s", abs(lon[i]), if (is.na(lon[i]) || lon[i] > 0) "E" else "W")
	rval
}

