summary.tide <- function(object, p=0, constituent, ...)
{
    n <- length(object$p)
    if (!missing(p)) ok <- (object$p <= p) else ok = seq(1, n)
    sig <- vector(length=n)
    sig <- rep("", n)
    sig[object$p<0.1]   <- ".  "
    sig[object$p<0.05]  <- "*  "
    sig[object$p<0.01]  <- "** "
    sig[object$p<0.001] <- "***"
    if (missing(constituent)) {
        rval <- data.frame(Name=object$name[ok], Frequency=object$frequency[ok],
                           Amplitude=object$amplitude[ok],	Phase=object$phase[ok],
                           p=object$p[ok], sig=sig[ok])
    }
    else {
        i <- which(object$name==constituent)
        if (length(i) == 0) stop("there is no such constituent '", constituent, "'")
        rval <- data.frame(Name=object$name[i], Frequency=object$frequency[i],
                           Amplitude=object$amplitude[i],	Phase=object$phase[i],
                           p=object$p[i], sig=sig[i])
    }
    class(rval) <- c("data.frame", "summary.tide")
    rval
}
