plot.rbrtdr <- function (x, which=1:4, ...)
{
    if (!inherits(x, "rbrtdr")) stop("method is only for rbrtdr objects")
    show <- rep(FALSE, 3)
    show[which] <- TRUE
    oldpar <- par(no.readonly = TRUE)
    lw <- length(which)
    if (!"mgp" %in% names(list(...))) par(mgp = c(2, 2/3, 0))
    if (lw == 1) {
        ;
    } else if (lw == 2) {
        par(mar=c(3,3,1,1))
        par(mfcol=c(2,1))
    } else if (lw==3 || lw==4) {
        par(mar=c(3,3,1,1))
        par(mfcol=c(2,2))
    }
    if (show[1]) {
        plot(x$data$t, x$data$temperature,
             xlab="", ylab=expression(paste("Temperature [ ", degree, "C ]")), type='l', ...)
    }
    if (show[2]) {
        plot(x$data$t, x$data$pressure,
             xlab="", ylab="p [dbar]", type='l',
             ylim=rev(range(x$data$pressure)),
             ...)
    }
    if (show[4]) {
        text.item <- function(item, cex=1) {
            if (!is.null(item) && !is.na(item)) {
                text(xloc, yloc, item, adj = c(0, 0), cex=cex);
                yloc <<- yloc - d.yloc;
            }
        }
        xfake <- seq(0:10)
        yfake <- seq(0:10)
        mar <- par(mar)
        par(mar=c(0,0,0,0))
        plot(xfake, yfake, type = "n", xlab = "", ylab = "", axes = FALSE)
        xloc <- 1
        yloc <- 10
        d.yloc <- 0.7
        cex <- 1
        text.item(paste("Instrument Serial Number: ", x$metadata$serial.number), cex=cex)
        text.item(paste("Sample period:", x$metadata$sample.period, "s"), cex=cex)
        par(mar=mar)
    }
    if (show[3]) {
        if ("cex" %in% names(list(...))) {
            plot(x$data$temperature, x$data$pressure,
                 xlim=rev(range(x$data$pressure)),
                 xlab=expression(paste("Temperature [", degree, "C ]")),
                 ylab="p [dbar]", type='p', ...)
        } else {
            plot(x$data$temperature, x$data$pressure,
                 xlim=rev(range(x$data$pressure)),
                 xlab=expression(paste("Temperature [", degree, "C ]")),
                 ylab="p [dbar]", type='p', cex=0.3, ...)
        }
    }
    if (lw != 1) par(oldpar)
    invisible()
}
