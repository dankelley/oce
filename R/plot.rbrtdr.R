plot.rbrtdr <- function (x, which=1:3, ...)
{
    if (!inherits(x, "rbrtdr")) stop("method is only for rbrtdr objects")
    show <- rep(FALSE, 3)
    show[which] <- TRUE
    lw <- length(which)
    if (lw == 2) {
        oldpar <- par(no.readonly = TRUE)
        par(mfrow=c(2,1))
    } else if (lw==3) {
        oldpar <- par(no.readonly = TRUE)
        par(mfrow=c(2,2))
    }
    if (show[1]) {
        plot(x$data$t, x$data$temperature,
             xlab="Time", ylab=expression(paste("Temperature [ ", degree, "C ]")), type='l', ...)
    }
    if (show[2]) {
        plot(x$data$t, x$data$pressure,
             xlab="Time", ylab="p [dbar]", type='l', ...)
    }
    if (show[3]) {
        plot(x$data$temperature, x$data$pressure,
             xlab=expression(paste("Temperature [", degree, "C ]")),
             ylab="p [dbar]", type='p', ...)
    }
    if (lw != 1) par(oldpar)
    invisible()
}
