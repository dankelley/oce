section.subset <- function(section, indices=1:length(section$data$station))
{
	n <- length(indices)
	station <- vector("list", n)
	stn <- vector("character", n)
	lon <- vector("numeric", n)
	lat <- vector("numeric", n)
	for (i in 1:n) {
		ii <- indices[i]
		stn[i] <- section$metadata$station.id[ii]
		lat[i] <- section$metadata$latitude[ii]
		lon[i] <- section$metadata$longitude[ii]
		station[[i]] <- section$data$station[[ii]]
	}
	data <- data.frame(station=station)
	metadata <- list(header=section$header,section.id=section$section.id,station.id=stn,latitude=lat,longitude=lon)
	log.item <- list(time = c(Sys.time()),
		paste("modified by section.subset(x, indices=c(",paste(indices,collapse=","),"))",sep=""))
	res <- list(data=data, metadata=metadata, processing.log=section$processing.log)
	class(res) <- "section"
	res <- processing.log.append(res, log.item)
	res
}
