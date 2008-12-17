undrift.time <- function(x, slow.end = 0, tname="t")
{
    if (!inherits(x, "oce")) stop("method is only for oce objects")
    names <- names(x$data)
    if (!(tname %in% names)) stop("no column named '", tname, "'; only found: ", paste(names," "))
    rval <- x
    t <- rval$data[[tname]]
    nt <- length(t)
    if (nt < 2) warning("too few data to to undrift time; returning object unaltered")
    else {
        sample.interval <- as.numeric(difftime(t[2], t[1], units="s"))
        nt <- length(t)
        nt.out <- floor(0.5 + nt + slow.end / sample.interval)
        t.out <- seq.POSIXt(from=t[1], by=sample.interval, length.out=nt.out)
        i <- seq(from=1, by=1, length.out=nt)
        i.out <- seq(from=1, to=nt, length.out = nt.out)
        out <- data.frame(array(dim=c(nt.out, dim(x$data)[2])))
        names(out) <- names
        out[[tname]] <- t.out
        for (name in names) {
            if (name != tname) {
                yy <- approx(x=i, y=x$data[[name]], xout=i.out)$y
                out[[name]] <- yy
            }
        }
        rval$data <- out
    }
    rval <- processing.log.append(rval, paste(deparse(match.call()), sep="", collapse=""))
    rval
}
