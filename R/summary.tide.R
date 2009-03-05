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
        fit <- data.frame(Const=object$const[ok],
                          Name=object$name[ok],
                          Freq=object$freq[ok],
                          Amplitude=sprintf("%10.5f", object$amplitude[ok]),
                          Phase=sprintf("%9.1f", object$phase[ok]),
                          p=sprintf("%9.4f", object$p[ok]),
                          sig=sig[ok])
    }
    else {
        i <- which(object$name==constituent)
        if (length(i) == 0) stop("there is no such constituent '", constituent, "'")
        fit <- data.frame(Const=object$const[i],
                          Name=object$name[i],
                          Freq=object$freq[i],
                          Amplitude=sprintf("%10.5f", object$amplitude[i]),
                          Phase=sprintf("%9.1f", object$phase[i]),
                          p=sprintf("%9.4f", object$p[i]),
                          sig=sig[i])
    }
    rval <- list(fit=fit, start.time=object$start.time, call="call")
    class(rval) <- c("summary.tidem")
    rval
}

print.summary.tidem <- function(x, digits = max(3, getOption("digits") - 3),
                                signif.start = getOption("show.signif.stars"),
                                ...)
{
    cat("\nCall:\n")
    cat(paste(deparse(x$call), sep="\n", collapse="\n"), "\n\n", sep="")
    print(x$fit)
    cat("Start time:", x$start.time, "\n")
}
