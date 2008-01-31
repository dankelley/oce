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
        rval <- data.frame(Const=object$const[ok],
                           Name=object$name[ok],
                           Freq=object$freq[ok],
                           Amplitude=format(object$amplitude[ok], digits=3),
                           Phase=format(object$phase[ok], digits=2),
                           p=format(object$p[ok], digits=3),
                           sig=sig[ok])
    }
    else {
        i <- which(object$name==constituent)
        if (length(i) == 0) stop("there is no such constituent '", constituent, "'")
        rval <- data.frame(Const=object$const[i],
                           Name=object$name[i],
                           Freq=object$freq[i],
                           Amplitude=format(object$amplitude[i], digits=3),
                           Phase=format(object$phase[i], digits=2),
                           p=format(object$p[i], digits=3),
                           sig=sig[i])
    }
    class(rval) <- c("data.frame", "summary.tide")
    rval
}
