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
    data <- list(station=station)
    metadata <- list(header=section$header,section.id=section$section.id,station.id=stn,latitude=lat,longitude=lon)
    res <- list(data=data, metadata=metadata, processing.log=section$processing.log)
    class(res) <- c("section", "oce")
    res <- processing.log.append(res, paste(deparse(match.call()), sep="", collapse=""))
    res
}
