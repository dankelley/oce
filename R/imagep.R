imagep <- function(x, y, z,
                   xlim, ylim, zlim,
                   flip.y=FALSE,
                   xlab="", ylab="", zlab="",
                   breaks, col,
                   do.layout=TRUE,
                   draw.contours=TRUE,
                   draw.time.range=getOption("oce.draw.time.range"),
                   mgp=getOption("oce.mgp"),
                   mar=c(mgp[1]+if(nchar(xlab)>0) 1.5 else 1, mgp[1]+if(nchar(ylab)>0) 1.5 else 1, mgp[2]+1/2, 1/2),
                   xaxs="i",
                   yaxs="i",
                   cex=par("cex"),
                   adorn,
                   axes=TRUE,
                   debug=getOption("oce.debug"),
                   ...)
{
    if (missing(x)) stop("must supply x")
    if (missing(y)) stop("must supply y")
    if (missing(z)) stop("must supply z")
    dim <- dim(z)
    if (dim[1] != length(x)) stop("dim(z)[1] must equal length(x)")
    if (dim[2] != length(y)) stop("dim(z)[2] must equal length(y)")
    par(mgp=mgp, mar=mar, cex=cex)
    w <- 1.5 # (1.5 + par("mgp")[2]) * par("csi") * 2.54 + 0.5

    if (do.layout)
        layout(matrix(1:2, nrow=1, byrow=TRUE), widths=c(1, lcm(w)))

    if (missing(breaks)) {
        zrange <- range(z, na.rm=TRUE)
        if (missing(zlim)) {
            if (missing(col))
                breaks <- pretty(zrange)
            else
                breaks <- seq(zrange[1], zrange[2], length.out=1+length(col))
            breaks.orig <- breaks
        } else {
            if (missing(col))
                breaks <- pretty(zlim)
            else
                breaks <- seq(zlim[1], zlim[2], length.out=1+length(col))
            breaks.orig <- breaks
            breaks[1] <- zrange[1]
            breaks[length(breaks)] <- zrange[2]
        }
    } else {
        breaks.orig <- breaks
    }
    if (missing(col))
        col <- oce.colors.palette(n=length(breaks)-1)

    x.is.time <- inherits(x, "POSIXt") || inherits(x, "POSIXct") || inherits(x, "POSIXlt")

    ylim <- if (missing(ylim)) range(y,na.rm=TRUE) else ylim
    if (flip.y) ylim <- rev(ylim)

    xlim <- if (missing(xlim)) range(x,na.rm=TRUE) else xlim

    plot.window(xlim=xlim, ylim=ylim)

    ## 1. plot main window
    if (x.is.time) {
        if (missing(breaks)) {
            image(x=x, y=y, z=z, axes=FALSE, xlab="", ylab=ylab, col=col,
                  xlim=if(missing(xlim))range(x,na.rm=TRUE) else xlim,
                  ylim=if(missing(ylim))range(y,na.rm=TRUE) else ylim,
                  zlim=if(missing(zlim))range(z,na.rm=TRUE) else zlim,
                  ...)
        } else {
            image(x=x, y=y, z=z, axes=FALSE, xlab="", ylab=ylab, breaks=breaks, col=col,
                  xlim=if(missing(xlim))range(x,na.rm=TRUE) else xlim,
                  ylim=if(missing(ylim))range(y,na.rm=TRUE) else ylim,
                  zlim=if(missing(zlim))range(z,na.rm=TRUE) else zlim,
                  ...)
        }
        box()
        if (axes) {
            oce.axis.POSIXct(side=1, x=x, cex.axis=cex, cex.lab=cex, draw.time.range=draw.time.range)
            axis(2, cex.axis=cex, cex.lab=cex)
        }
    } else {
        if (missing(breaks)) {
            image(x=x, y=y, z=z, axes=FALSE, xlab=xlab, ylab=ylab, col=col,
                  xlim=if(missing(xlim))range(x,na.rm=TRUE) else xlim,
                  ylim=if(missing(ylim))range(y,na.rm=TRUE) else ylim,
                  zlim=if(missing(zlim))range(z,na.rm=TRUE) else zlim,
                  ...)
        } else {
            image(x=x, y=y, z=z, axes=FALSE, xlab=xlab, ylab=ylab, breaks=breaks, col=col,
                  xlim=if(missing(xlim))range(x,na.rm=TRUE) else xlim,
                  ylim=if(missing(ylim))range(y,na.rm=TRUE) else ylim,
                  zlim=if(missing(zlim))range(z,na.rm=TRUE) else zlim,
                  ...)
        }
        box()
        if (axes) {
            axis(1, cex.axis=cex, cex.lab=cex)
            axis(2, cex.axis=cex, cex.lab=cex)
        }
    }
    if (draw.contours && !missing(breaks))
        contour(x=x, y=y, z=z, levels=breaks, drawlabels=FALSE, add=TRUE, col="black")
    mtext(zlab, side=3, cex=par("cex"), adj=1, line=1/8)

    if (!missing(adorn)) {
        t <- try(eval.parent(adorn), silent=!TRUE)
        if (class(t) == "try-error") warning("cannot evaluate adorn='", adorn, "'\n")
    }
    oce.debug(debug, "axes=", axes, "\n")

    ## 2. plot palette
    par(mar=c(mar[1], 1/4, mgp[2]+1/2, mgp[2]+1))
    if (missing(breaks)) {
        if (missing(zlim)) {
            palette <- seq(min(z, na.rm=TRUE), max(z, na.rm=TRUE), length.out=300)
        } else {
            palette <- seq(zlim[1], zlim[2], length.out=300)
        }
        image(x=1, y=palette, z=matrix(palette, nrow=1), axes=FALSE, xlab="", ylab="", col=col,
              cex=cex, cex.axis=cex, cex.lab=cex,
              zlim=if(missing(zlim))range(z,na.rm=TRUE) else zlim)
    } else {
        if (missing(zlim)) {
            palette <- seq(breaks[1], breaks[length(breaks)], length.out=300)
        } else {
            palette <- seq(zlim[1], zlim[2], length.out=300)
        }
        image(x=1, y=palette, z=matrix(palette, nrow=1), axes=FALSE, xlab="", ylab="",
              breaks=breaks.orig,
              col=col,
              zlim=if(missing(zlim))range(z,na.rm=TRUE) else zlim)
    }
    if (draw.contours && !missing(breaks))
        abline(h=breaks)
    box()
    axis(side=4, at=pretty(palette), cex.axis=cex)
}
