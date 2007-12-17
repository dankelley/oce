as.coastline <- function(latitude, longitude)
{
	n <- length(latitude)
	if (n != length(longitude))
		stop("Lengths of longitude and latitude must be equal")
	data <- list(longitude=longitude, latitude=latitude)
	log <- list(time=c(Sys.time()), action=c("created by as.coastline()"))
	res <- list(data=data, metadata=NULL, log=log)
	class(res) <- "coastline"
	res
}
