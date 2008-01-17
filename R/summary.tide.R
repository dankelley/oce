summary.tide <- function(object, p=0, ...)
{
	n <- length(object$p)
	if (!missing(p)) ok <- (object$p <= p) else ok = seq(1, n)
	sig <- vector(length=n)
	sig <- rep("", n)
	sig[object$p<0.1]   <- ".  "
	sig[object$p<0.05]  <- "*  "
	sig[object$p<0.01]  <- "** "
	sig[object$p<0.001] <- "***"
	rval <- data.frame(Name=object$name[ok], Frequency=object$frequency[ok],
                       Amplitude=object$amplitude[ok],	Phase=object$phase[ok],
                       p=object$p[ok], sig=sig[ok]) # FIXME: do these p values make any sense?
	class(rval) <- c("data.frame", "summary.tide")
	rval
}
