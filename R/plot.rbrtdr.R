plot.rbrtdr <- function (x, which=1:3, ...)
{
    if (!inherits(x, "rbrtdr")) stop("method is only for rbrtdr objects")
    show <- rep(FALSE, 3)
    show[which] <- TRUE
    lw <- length(which)
    if (lw == 2) {
        oldpar <- par(no.readonly = TRUE)
        par(mfcol=c(2,1))
        par(mar=c(3,3,1,1))
        if (!"mgp" %in% names(list(...))) par(mgp = c(2, 2/3, 0))
    } else if (lw==3) {
        oldpar <- par(no.readonly = TRUE)
        par(mfcol=c(2,2))
        par(mar=c(3,3,1,1))
        if (!"mgp" %in% names(list(...))) par(mgp = c(2, 2/3, 0))
    }
    if (show[1]) {
        plot(x$data$t, x$data$temperature,
             xlab="", ylab=expression(paste("Temperature [ ", degree, "C ]")), type='l', ...)
    }
    if (show[2]) {
        plot(x$data$t, x$data$pressure,
             xlab="", ylab="p [dbar]", type='l', ...)
    }

    if (show[3]) {
        if ("cex" %in% names(list(...))) {
            plot(x$data$temperature, x$data$pressure,
                 xlab=expression(paste("Temperature [", degree, "C ]")),
                 ylab="p [dbar]", type='p', ...)
        } else {
            plot(x$data$temperature, x$data$pressure,
                 xlab=expression(paste("Temperature [", degree, "C ]")),
                 ylab="p [dbar]", type='p', cex=0.3, ...)
        }
    }
    if (lw != 1) par(oldpar)
    invisible()
}
