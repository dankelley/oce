as.sealevel <- function(
                        eta,
                        t,
                        header=NULL,
                        station.number=NA,
                        station.version=NA,
                        station.name=NULL,
                        region=NA,
                        year=NA,
                        latitude=NA,
                        longitude=NA,
                        GMT.offset=NA,
                        decimation.method=NA,
                        reference.offset=NA,
                        reference.code=NA,
                        deltat)
{
    if (missing(eta)) stop("must supply sealevel height, eta, in metres")
    n <- length(eta)
    if (missing(t)) {
                                        # construct hourly from time "zero"
        start <- as.POSIXct("0000-01-01 00:00:00", tz="GMT")
        t <- as.POSIXct(start + seq(0, n - 1, 1) * 3600, tz="GMT")
        if (is.na(GMT.offset)) GMT.offset <- 0 # FIXME: do I want to do this?
    } else {
        t <- as.POSIXct(t, tz="GMT") # FIXME: should this be GMT?
    }
    data <- data.frame(t=t, eta=eta)
    if (missing(deltat))
        deltat <- difftime(t[2], t[1], units="hours")
    if (is.na(deltat) | deltat <= 0)
        deltat <- 1
    metadata <- list(header=header,
                     year=year,
                     station.number=station.number,
                     station.version=station.version,
                     station.name=station.name,
                     region=region,
                     latitude=latitude,
                     longitude=longitude,
                     GMT.offset=GMT.offset,
                     decimation.method=decimation.method,
                     reference.offset=reference.offset,
                     reference.code=reference.code,
                     units=units,
                     n=length(t),
                     deltat=deltat)
    log.item <- processing.log.item(paste(deparse(match.call()), sep="", collapse=""))
    rval <- list(data=data, metadata=metadata, processing.log=log.item)
    class(rval) <- c("sealevel", "oce")
    rval
}
