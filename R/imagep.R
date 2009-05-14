imagep <- function(x, y, z,
                   xlim, ylim, zlim,
                   xlab="", ylab="", zlab="",
                   col, breaks,
                   do.layout=TRUE,
                   draw.contours=TRUE,
                   draw.time.range=TRUE,
                   mgp=getOption("oce.mgp"),
                   xaxs = "i", yaxs = "i",
                   ...)
{
    if (missing(x)) stop("must supply x")
    if (missing(y)) stop("must supply y")
    if (missing(z)) stop("must supply z")
    dim <- dim(z)
    if (dim[1] != length(x)) stop("dim(z)[1] must equal length(x)")
    if (dim[2] != length(y)) stop("dim(z)[2] must equal length(y)")
    w <- (1.0+par("mgp")[1]) * par("csi") * 2.54
    par(mgp=mgp)
    par(mar=c(mgp[1]+if(nchar(xlab)>0) 1 else 0,
        mgp[1]+if(nchar(ylab)>0) 1 else 0,
        mgp[2]+1/2,
        1/4))
    if (do.layout)
        layout(matrix(1:2, nrow=1, byrow=TRUE), widths=c(1, lcm(w)))
    if (missing(breaks))
        breaks <- pretty(z)
    col <- oce.colors.palette(n=length(breaks)-1)
    if (inherits(x, "POSIXt")) {
        image(x=x, y=y, z=z, axes=FALSE, xlab="", ylab=resizable.label("p", "y"), breaks=breaks, col=col)
        oce.axis.POSIXct(side=1, x=x)
        box()
        axis(2)
    } else {
        image(x=x, y=y, z=z, xlab=xlab, ylab=ylab, breaks=breaks, col=col)
    }
    if (draw.time.range && inherits(x, "POSIXt"))
        mtext(paste(paste(format(range(time)), collapse=" to "), attr(time[1], "tzone")), side=3, cex=3/4*par("cex.axis"), adj=0)
    if (draw.contours)
        contour(x=x, y=y, z=z, levels=breaks, drawlabels=FALSE, add=TRUE, col="black")
    mtext(zlab, side=3, cex=1, adj=1)
    ## palette
    par(mar=c(mgp[1]+if(nchar(xlab)>0) 1 else 0,
        1/4,
        mgp[2]+1/2,
        mgp[2]+1))
    palette <- seq(breaks[1], breaks[length(breaks)], length.out=300)
    image(x=1, y=palette, z=matrix(palette, nrow=1), axes=FALSE, xlab="", ylab="", breaks=breaks, col=col, cex=3/4)
    if (draw.contours)
        abline(h=breaks)
    box()
    axis(side=4, at=pretty(palette))
}
