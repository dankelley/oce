imagep <- function(x, y, z,
                   xlim, ylim, zlim,
                   flip.y=FALSE,
                   xlab="", ylab="", zlab="",
                   breaks, col,
                   do.layout=TRUE,
                   draw.contours=TRUE,
                   draw.time.range=TRUE,
                   mgp=getOption("oce.mgp"),
                   xaxs = "i", yaxs = "i", cex=par("cex"),
                   ...)
{
    if (missing(x)) stop("must supply x")
    if (missing(y)) stop("must supply y")
    if (missing(z)) stop("must supply z")
    dim <- dim(z)
    if (dim[1] != length(x)) stop("dim(z)[1] must equal length(x)")
    if (dim[2] != length(y)) stop("dim(z)[2] must equal length(y)")
    par(mgp=mgp)
    w <- (1.5 + par("mgp")[2]) * par("csi") * 2.54 + 0.5
    par(mar=c(mgp[2]+if(nchar(xlab)>0) 1.5 else 0,
        mgp[1]+if(nchar(ylab)>0) 1.5 else 0,
        mgp[2]+1/2,
        1/4))

    ##cat("in imagep(), w=",w,"mgp=",paste(mgp, collapse=" "), "mar=",paste(par("mar"), collapse=" "), "\n")

    if (do.layout)
        layout(matrix(1:2, nrow=1, byrow=TRUE), widths=c(1, lcm(w)))
    if (missing(breaks))
        col <- oce.colors.palette(n=256)
    else
        col <- oce.colors.palette(n=length(breaks)-1)
    x.is.time <- inherits(x, "POSIXt") || inherits(x, "POSIXct") || inherits(x, "POSIXlt")

    ylim <- if (missing(ylim)) range(y) else ylim
    if (flip.y) ylim <- rev(ylim)

    xlim <- if (missing(xlim)) range(x) else xlim

    plot.window(xlim=xlim, ylim=ylim)

    if (x.is.time) {
        if (missing(breaks)) {
            image(x=x, y=y, z=z, axes=FALSE, xlab="", ylab=ylab, col=col,
                  xlim=if(missing(xlim))range(x) else xlim,
                  ylim=if(missing(ylim))range(y) else ylim,
                  zlim=if(missing(zlim))range(z) else zlim,
                  ...)
        } else {
            image(x=x, y=y, z=z, axes=FALSE, xlab="", ylab=ylab, breaks=breaks, col=col,
                  xlim=if(missing(xlim))range(x) else xlim,
                  ylim=if(missing(ylim))range(y) else ylim,
                  zlim=if(missing(zlim))range(z) else zlim,
                  ...)
        }
        oce.axis.POSIXct(side=1, x=x)
        box()
        axis(2)
    } else {
        if (missing(breaks)) {
            image(x=x, y=y, z=z, xlab=xlab, ylab=ylab, col=col,
                  xlim=if(missing(xlim))range(x) else xlim,
                  ylim=if(missing(ylim))range(y) else ylim,
                  zlim=if(missing(zlim))range(z) else zlim,
                  ...)
        } else {
            image(x=x, y=y, z=z, xlab=xlab, ylab=ylab, breaks=breaks, col=col,
                  xlim=if(missing(xlim))range(x) else xlim,
                  ylim=if(missing(ylim))range(y) else ylim,
                  zlim=if(missing(zlim))range(z) else zlim,
                  ...)
        }
    }
    if (draw.time.range && x.is.time)
        mtext(paste(paste(format(range(x)), collapse=" to "), attr(x[1], "tzone")), side=3, cex=5/6*par("cex"), adj=0)
    if (draw.contours && !missing(breaks))
        contour(x=x, y=y, z=z, levels=breaks, drawlabels=FALSE, add=TRUE, col="black")
    mtext(zlab, side=3, cex=par("cex")*5/4, adj=1, line=1/6)
    ## palette
    par(mar=c(mgp[2]+if(nchar(xlab)>0) 1.5 else 0,
        1/4,
        mgp[2]+1/2,
        mgp[2]+1))
    if (missing(breaks)) {
        if (missing(zlim))
            palette <- seq(min(z, na.rm=TRUE), max(z, na.rm=TRUE), length.out=300)
        else
            palette <- seq(zlim[1], zlim[2], length.out=300)
        image(x=1, y=palette, z=matrix(palette, nrow=1), axes=FALSE, xlab="", ylab="", col=col,
              cex=par("cex"),
              zlim=if(missing(zlim))range(z) else zlim)
    } else {
        if (missing(zlim))
            palette <- seq(breaks[1], breaks[length(breaks)], length.out=300)
        else
            palette <- seq(zlim[1], zlim[2], length.out=300)
        image(x=1, y=palette, z=matrix(palette, nrow=1), axes=FALSE, xlab="", ylab="", breaks=breaks, col=col,
              cex=par("cex"),
              zlim=if(missing(zlim))range(z) else zlim)
    }
    if (draw.contours && !missing(breaks))
        abline(h=breaks)
    box()
    axis(side=4, at=pretty(palette))
}
