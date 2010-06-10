#imagep(h, drho, speed, xlab="Equivalent depth [m]",ylab=expression(paste(Delta*rho, " [kg/m^3]")),zlab="Internal-wave speed [m/s]")
#quartz()
#source('~/src/R-kelley/oce/R/imagepnew.R');imagepnew(h, drho, speed, xlab="Equivalent depth [m]",ylab=expression(paste(Delta*rho, " [kg/m^3]")),zlab="Internal-wave speed [m/s]",debug=4)

imagepnew <- function(x, y, z,
                      xlim, ylim, zlim,
                      flip.y=FALSE,
                      xlab="", ylab="", zlab="",
                      breaks, col,
                      draw.contours=TRUE, # FIXME
                      draw.time.range=getOption("oce.draw.time.range"),
                      mgp=getOption("oce.mgp"),
                      mar=c(mgp[1]+if(nchar(xlab)>0) 1.5 else 1, mgp[1]+if(nchar(ylab)>0) 1.5 else 1, mgp[2]+1/2, 1/2),
                      xaxs = "i", yaxs = "i",
                      cex=par("cex"),
                      adorn,
                      axes=TRUE,
                      debug=getOption("oce.debug"),
                      ...)
{
    oce.debug(debug, "at top of new.imagep(), par('cex') is", par('cex'), "\n")
    if (missing(x)) stop("must supply x")
    if (missing(y)) stop("must supply y")
    if (missing(z)) stop("must supply z")
    dim <- dim(z)
    if (dim[1] != length(x)) stop("dim(z)[1] must equal length(x)")
    if (dim[2] != length(y)) stop("dim(z)[2] must equal length(y)")

    # set overall graphical parameters (note: get opai after setting mar)
    par(mgp=mgp, mar=mar, cex=cex)
    omai <- par("mai")
    opin <- par("pin")
    if (debug > 0)
        cat("paper geometry:", paste(opin, collapse="x"), "\n")


    line.height <- 1.5*par("cin")[2]        # inches (not sure on this ... this is character height)
    tic.length <- abs(par("tcl")) * line.height # inches (not sure on this)


    ## widths of items [in inches]
    widths <- list(                         # widths
                   margin.lhs=omai[2], # width of LHS margin
                   main=NA,           # main image width
                   palette.separation=1/8,  # between main & palette
                   palette.width=1/4,       # palette width
                   ##margin.rhs=omai[1]) # width of RHS margin (fixme: more space than needed)
                   margin.rhs=line.height+tic.length) # width of RHS margin (fixme: more space than needed)
    widths$main <- opin[1] - widths$margin.lhs - widths$palette.separation - widths$palette.width - widths$margin.rhs # doing this here ensures things add up

    gave.breaks <- !missing(breaks)
    if (!gave.breaks) {
        zrange <- range(z, na.rm=TRUE)
        if (missing(zlim)) {
            if (missing(col))
                breaks <- pretty(zrange)
            else
                breaks <- seq(zrange[1], zrange[2],
                              length.out=if(is.function(col))128 else 1+length(col))
            breaks.orig <- breaks
        } else {
            if (missing(col))
                breaks <- pretty(zlim)
            else
                breaks <- seq(zlim[1], zlim[2],
                              length.out=if(is.function(col))128 else 1+length(col))
            breaks.orig <- breaks
            breaks[1] <- zrange[1]
            breaks[length(breaks)] <- zrange[2]
        }
    } else {
        breaks.orig <- breaks
    }
    if (missing(col))
        col <- oce.colors.palette(n=length(breaks)-1)
    if (is.function(col))
        col <- col(n=length(breaks)-1)

    x.is.time <- inherits(x, "POSIXt") || inherits(x, "POSIXct") || inherits(x, "POSIXlt")

    ## FIXME: should draw palette first, perhaps, to let users add to main plot

    ## main image
    the.mai <- c(omai[1],
                 widths$margin.lhs,
                 omai[3],
                 widths$palette.separation + widths$palette.width + widths$margin.rhs)
    if (debug > 0)
        str(widths)
    oce.debug(debug, "original value of par(mai)=", paste(omai), "\n")
    oce.debug(debug, "MAIN: setting     par(mai)=", paste(the.mai), "\n")
    par(mai=the.mai)
    if (x.is.time) {
        if (!gave.breaks) {
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
        if (!gave.breaks) {
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

    ## palette
    the.mai <- c(omai[1],
                 2*widths$margin.lhs + widths$main + 2*widths$palette.separation, # FIXME: why do the "2*" work?
                 omai[3],
                 widths$margin.rhs)
    oce.debug(debug, "PALETTE: setting  par(mai)=", paste(the.mai), "\n")
    par(new=TRUE, mai=the.mai, cex=cex)
    ##plot(1:10, 1:10);return()
    if (!gave.breaks) {
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
    if (draw.contours && gave.breaks)   #FIXME imagep() different (and this one fails to give contours in palette)
        abline(h=breaks)
    box()
    axis(side=4, at=pretty(palette), cex.axis=cex)
    par(mai=omai)                       # FIXME: do I really want this?
}
