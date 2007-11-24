latlon.format <- function(lat, lon) {
	if (is.na(lat) || is.na(lon))
		""
	else
		sprintf("%8.4f%1s %8.4f%1s",
			abs(lat), if (lat > 0) "N" else "S",
			abs(lon), if (lon > 0) "E" else "W")
}
lat.format <- function(lat) {
	if (is.na(lat))
		""
	else
		sprintf("%8.4f%1s", abs(lat), if (is.na(lat) || lat > 0) "N" else "S")
}
lon.format <- function(lon) {
	if (is.na(lon))
		""
	else 
		sprintf("%8.4f%1s",	abs(lon), if (is.na(lon) || lon > 0) "E" else "W")
}

