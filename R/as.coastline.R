as.coastline <- function(latitude, longitude)
{
	n <- length(latitude)
	if (n != length(longitude))
		stop("Lengths of longitude and latitude must be equal")
	processing.log <- list(time=c(Sys.time()), action=c("created by as.coastline()"))
	data <- list(longitude=longitude, latitude=latitude)
	res <- list(processing.log=processing.log, data=data)
	class(res) <- "coastline"
	res
}
