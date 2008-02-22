plot.TS <- function (x,
                     rho.levels = 6,
                     grid = FALSE,
                     col.grid = "lightgray",
                     rho1000=FALSE,
                     col = par("col"),
                     col.rho = "darkgray",
                     cex.rho = 0.9 * par("cex"),
                     cex=par("cex"),
                     pch=20,
                     rotate.rho.labels=FALSE,
                     connect.points=FALSE,
                     ...)
{
    if (!inherits(x, "ctd")) stop("method is only for ctd objects")
    mar <- par("mar")
    if (!rotate.rho.labels) { # make space
        if (mar[4] < 3)
            par(mar=c(mar[1:3], 3))
    }
    args <- list(...)
    if (is.null(args$xlab) && is.null(args$ylab)) {
        plot(x$data$salinity, x$data$temperature, xlab = "",
             xaxs = if (min(x$data$salinity,na.rm=TRUE)==0) "i" else "r", # avoid plotting S<0
             ylab = expression(paste("Temperature [ ", degree, "C ]")),
             cex=cex, pch=pch, col=col, ...)
    } else {
        plot(x$data$salinity, x$data$temperature,
# xlab = args$xlab,
             xaxs = if (min(x$data$salinity,na.rm=TRUE)==0) "i" else "r", # avoid plotting S<0
#             ylab = args$ylab,
             cex=cex, pch=pch, col=col, ...)
    }
    if (connect.points) lines(x$data$salinity, x$data$temperature, col=col, ...)
    S.axis.min <- par()$usr[1]
    S.axis.max <- par()$usr[2]
    T.axis.min <- par()$usr[3]
    T.axis.max <- par()$usr[4]
    if (is.null(args$xlab)) mtext("Salinity [ PSU ]", side = 1, line = 3)
    if (grid) grid(col="lightgray")
    rho.min <- sw.sigma(max(0,S.axis.min), T.axis.max, 0)
    rho.max <- sw.sigma(S.axis.max, T.axis.min, 0)
    if (length(rho.levels) == 1) {
        rho.list <- pretty(c(rho.min, rho.max), n=rho.levels)
                                        # Trim first and last values, since not in box
        rho.list <- rho.list[-1]
        rho.list <- rho.list[-length(rho.list)]
    } else {
        rho.list <- rho.levels
    }
    t.n <- 300
    t.line <- seq(T.axis.min, T.axis.max, length.out=t.n)
    for (rho in rho.list) {
        rho.label <- if (rho1000) 1000+rho else rho
        s.line <- sw.S.T.rho(t.line, rep(rho, t.n), rep(0, t.n))
        ok <- !is.na(s.line) # crazy T can give crazy S
        s.ok <- s.line[ok]
        t.ok <- t.line[ok]
        lines(s.ok, t.ok, col = col.rho)
        if (s.ok[length(s.ok)] > S.axis.max) { # to right of box
            i <- match(TRUE, s.ok > S.axis.max)
            if (rotate.rho.labels)
                mtext(rho.label, side=4, at=t.line[i], line=0.25, cex=cex.rho, col=col.rho)
            else
                text(par("usr")[2], t.line[i], rho.label, pos=4, cex=cex.rho, col=col.rho, xpd=TRUE)
        } else { # above box ... if the line got there
            if (max(t.ok) > (T.axis.max - 0.05 * (T.axis.max - T.axis.min)))
                mtext(rho.label, side=3, at=s.line[t.n], line=0.25, cex=cex.rho, col=col.rho)
        }
    }
                                        # Freezing point
    Sr <- c(max(0, S.axis.min), S.axis.max)
    lines(Sr, sw.T.freeze(Sr, p=0), col="darkblue")
    par(mar=mar)
}
