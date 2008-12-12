subset.oce <- function (x, subset, indices=NULL, ...)
{
    if (inherits(x, "section")) {
        if (!is.null(indices)) {        # select a portion of the stations
            n <- length(indices)
            station <- vector("list", n)
            stn <- vector("character", n)
            lon <- vector("numeric", n)
            lat <- vector("numeric", n)
            for (i in 1:n) {
                ii <- indices[i]
                stn[i] <- x$metadata$station.id[ii]
                lat[i] <- x$metadata$latitude[ii]
                lon[i] <- x$metadata$longitude[ii]
                station[[i]] <- x$data$station[[ii]]
            }
            data <- list(station=station)
            metadata <- list(header=x$header,section.id=x$section.id,station.id=stn,latitude=lat,longitude=lon)
            rval <- list(data=data, metadata=metadata, processing.log=x$processing.log)
            class(rval) <- c("section", "oce")
        } else {                        # subset within the stations
            rval <- x
            n <- length(x$data$station)
            r <- eval(substitute(subset), x$data$station[[1]]$data, parent.frame())
            for (i in 1:n) {
                rval$data$station[[i]]$data <- x$data$station[[i]]$data[r,]
            }
        }
        rval <- processing.log.append(rval, paste(deparse(match.call()), sep="", collapse=""))
    } else {
        r <- eval(substitute(subset), x$data, parent.frame())
        r <- r & !is.na(r)
        rval <- x
        rval$data <- x$data[r,]
        rval <- processing.log.append(rval, paste(deparse(match.call()), sep="", collapse=""))
    }
    rval
}
