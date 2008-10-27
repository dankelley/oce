plot.rbrtdr <- function (x, ...)
{
    if (!inherits(x, "rbrtdr")) stop("method is only for rbrtdr objects")
    oldpar <- par(no.readonly = TRUE)
    par(mfrow=c(2,1))
    plot(x$data$t, x$data$temperature,
         xlab="Time", ylab=expression(paste("Temperature [ ", degree, "C ]")), type='l')
    plot(x$data$t, x$data$pressure,
         xlab="Time", ylab="p [dbar]", type='l')
    par(oldpar)
    invisible()
}
