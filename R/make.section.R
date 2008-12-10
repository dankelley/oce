make.section <- function(item, ...)
{
    if (inherits(item, "ctd")) {
        extra.args <- list(...)
        if (length(extra.args) < 1) stop("cannot make a section from one station")
        num.stations <- 1 + length(extra.args)
        station <- vector("list", num.stations)
        stn <- vector("character", num.stations)
        lon <- vector("numeric", num.stations)
        lat <- vector("numeric", num.stations)
        stn[1] <- item$metadata$station
        lat[1] <- item$metadata$latitude
        lon[1] <- item$metadata$longitude
        station[[1]] <- item
        for (i in 2:num.stations) {
            stn[i] <- extra.args[[i-1]]$metadata$station
            lat[i] <- extra.args[[i-1]]$metadata$latitude
            lon[i] <- extra.args[[i-1]]$metadata$longitude
            station[[i]] <- extra.args[[i-1]]
            ##summary(extra.args[i-1])
        }
    } else if (inherits(item, "list")) {
        num.stations <- length(item)
        if (num.stations < 2) {
            stop("cannot make a section from one station")
        }
        station <- vector("list", num.stations)
        stn <- vector("character", num.stations)
        lon <- vector("numeric", num.stations)
        lat <- vector("numeric", num.stations)
        for (i in 1:num.stations) {
            stn[i] <- item[[i]]$metadata$station
            lat[i] <- item[[i]]$metadata$latitude
            lon[i] <- item[[i]]$metadata$longitude
            station[[i]] <- item[[i]]
        }
    } else {
        stop("first argument must be of class \"ctd\" or a \"list\"")
    }
    data <- list(station=station)
    metadata <- list(header="",section.id="",station.id=stn,latitude=lat,longitude=lon)
    log.item <- list(time=c(Sys.time()), action=deparse(match.call()))
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("section", "oce")
    res
}
