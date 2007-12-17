section.subset <- function(section, indices=1:length(section$station))
{
	n <- length(indices)
    station <- vector("list", n)
	stn <- vector("character", n)
	lon <- vector("numeric", n)
	lat <- vector("numeric", n)
    for (i in 1:n) {
		ii <- indices[i]
		stn[i] <- section$station.id[ii]
		lat[i] <- section$latitude[ii]
		lon[i] <- section$longitude[ii]
		station[[i]] <- section$station[[ii]]
	}
    res <- list(header=section$header,
	 	section.id=section$section.id, 
		station.id=stn, latitude=lat, longitude=lon,
		station=station,
		log = section$log)
	log.item <- paste("modified by section.subset(x, indices=c(",paste(indices,collapse=","),"))",sep="")
    class(res) <- "section"
	res <- log.append(res, log.item)
	res
}
