plot.tdr <- function (x, which=1:4, ...)
{
    if (!inherits(x, "tdr")) stop("method is only for tdr objects")
    show <- rep(FALSE, 4)
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
             xlab="", ylab="Pressure [dbar]", type='l',
             ylim=rev(range(x$data$pressure, na.rm=TRUE)),
             ...)
    }
    if (show[4]) {
        text.item <- function(item, cex=1.25) {
            if (!is.null(item) && !is.na(item)) {
                text(xloc, yloc, item, adj = c(0, 0), cex=cex);
                yloc <<- yloc - d.yloc;
            }
        }
        xfake <- seq(0:10)
        yfake <- seq(0:10)
        mar <- par("mar")
        par(mar=c(0,0,0,0))
        plot(xfake, yfake, type = "n", xlab = "", ylab = "", axes = FALSE)
        xloc <- 1
        yloc <- 10
        d.yloc <- 0.7
        text.item(paste("Serial Number: ", x$metadata$serial.number),cex=1.25)
        text.item(paste("Start:", x$data$t[1]), cex=1)
        text.item(paste("End:", x$data$t[length(x$data$t)]), cex=1)
        text.item(paste("Sampling interval:", difftime(x$data$t[2], x$data$t[1], units="s"), "s"),cex=1)
        par(mar=mar)
    }
    if (show[3]) {
        args <- list(x=x$data$temperature, y=x$data$pressure,
                     ylim=rev(range(x$data$pressure, na.rm=TRUE)),
                     xlab=expression(paste("Temperature [", degree, "C ]")),
                     ylab="Pressure [dbar]", ...)
        if (!("type" %in% names(list(...)))) args <- c(args, type="p")
        if (!("cex"  %in% names(list(...)))) args <- c(args, cex=0.5)
        do.call(plot, args)
    }
    if (lw != 1) par(oldpar)
    invisible()
}
