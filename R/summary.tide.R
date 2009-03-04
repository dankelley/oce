summary.tide <- function(object, p, constituent, ...)
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
        rval <- data.frame(Const=object$const[ok],
                           Name=object$name[ok],
                           Freq=object$freq[ok],
                           Amplitude=sprintf("%10.5f", object$amplitude[ok]),
                           Phase=sprintf("%9.1f", object$phase[ok]),
                           p=sprintf("%9.4g", object$p[ok]),
                           sig=sig[ok])
    }
    else {
        i <- which(object$name==constituent)
        if (length(i) == 0) stop("there is no such constituent '", constituent, "'")
        rval <- data.frame(Const=object$const[i],
                           Name=object$name[i],
                           Freq=object$freq[i],
                           Amplitude=sprintf("%10.5f", object$amplitude[i]),
                           Phase=sprintf("%9.1f", object$phase[i]),
                           p=sprintf("%9.4g", object$p[i]),
                           sig=sig[i])
    }
    class(rval) <- c("data.frame", "summary.tide")
    rval
}
