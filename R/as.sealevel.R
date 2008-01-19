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
                        sampling.interval=NA)
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
    if (is.na(sampling.interval)) sampling.interval <-
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
                         sampling.interval=as.numeric(difftime(t[2], t[1], units="hours")))
    log.item <- list(time=c(Sys.time()), action=c("created by as.sealevel()"))
    rval <- list(data=data, metadata=metadata, processing.log=log.item)
    class(rval) <- c("sealevel", "oce")
    rval
}
