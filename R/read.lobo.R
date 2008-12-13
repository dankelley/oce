read.lobo <- function(file, cols=7, log.action) {
    header <- scan(file, what=character(), sep="\t", nlines=1, quiet=TRUE)
    d <- scan(file, what=character(), sep="\t", skip=1,  quiet=TRUE)
                                        # find columns. BUG: assumes names don't change
    col.date         <- grep("date", header)
    col.u            <- grep("current across", header)
    col.v            <- grep("current along", header)
    col.nitrate      <- grep("nitrate", header)
    col.fluorescence <- grep("fluorescence", header)
    col.salinity     <- grep("salinity", header)
    col.temperature  <- grep("temperature", header)
    if (cols == 7) {
        n <- length(d) / cols
        time         <-            d[seq(from=col.date,         by=cols, length.out = n)]
        u            <- as.numeric(d[seq(from=col.u,            by=cols, length.out = n)])
        v            <- as.numeric(d[seq(from=col.v,            by=cols, length.out = n)])
        nitrate      <- as.numeric(d[seq(from=col.nitrate,      by=cols, length.out = n)])
        fluorescence <- as.numeric(d[seq(from=col.fluorescence, by=cols, length.out = n)])
        salinity     <- as.numeric(d[seq(from=col.salinity,     by=cols, length.out = n)])
        temperature  <- as.numeric(d[seq(from=col.temperature,  by=cols, length.out = n)])
        p            <- rep(0, length(salinity))
        time <- as.POSIXlt(time)
        data <- data.frame(time=time,u=u,v=v,salinity=salinity,temperature=temperature,p=p,nitrate=nitrate,fluorescence=fluorescence)
        metadata <- list(header=header)
        if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
        log.item <- processing.log.item(log.action)
        res <- list(data=data, metadata=metadata, processing.log=log.item)
        class(res) = c("lobo", "oce")
        res
    } else {
        stop("debug: only working on one format right now")
    }
}
