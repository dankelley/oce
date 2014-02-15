\name{drawPalette}

\alias{drawPalette}

\title{Draw palette on RHS of plot device}

\description{Draw a palette on the right-hand side of plot device}

\usage{drawPalette(zlim, zlab="", breaks, col,
    labels=NULL, at=NULL, mai, fullpage=FALSE,
    drawContours=FALSE, drawTriangles=FALSE,
    cex.axis=par("cex.axis"),
    debug=getOption("oceDebug"), \dots)}

\arguments{
    \item{zlim}{two-element vector containing the lower and upper limits of z.
        (This may also be a vector of any length exceeding 1, in which case its
        range is used.) If not provided, the palette is not drawn, but space is
        set aside for it, so that the next plotting command has a right-hand
        margin matching the space that would have been occupied by the
        palette.}
    \item{zlab}{label for the palette scale.}
    \item{breaks}{the z values for breaks in the colour scheme.}
    \item{col}{either a vector of colours corresponding to the breaks, of length
        1 plus the number of breaks, or a function specifying colours,
        e.g. \code{\link{oceColorsJet}} for a rainbow.}
    \item{labels}{optional vector of labels for ticks on palette axis (must
        correspond with \code{at})}
    \item{at}{optional vector of positions for the \code{label}s}
    \item{mai}{margins for palette, as defined in the usual way; see
        \code{\link{par}}.  If not given, reasonable values are inferred from
        the existence of a non-blank \code{zlab}.}
    \item{fullpage}{logical value indicating whether to draw the palette
        filling the whole plot width (apart from \code{mai}, of course).  This
        can be helpful if the palette panel is to be created with
        \code{\link{layout}}, as illustrated in the \dQuote{Examples}.}

    \item{drawContours}{logical value indicating whether to draw contours on
        the palette, at the colour breaks.}

    \item{drawTriangles}{logical value indicating whether to draw triangles on
        the top and bottom of the palette.  If a single value is provide, it
        applies to both ends of the palette.  If a pair is provided, the first
        refers to the lower range of the palette, and the second to the upper
        range.}

    \item{debug}{a flag that turns on debugging.  Set to 1 to get a
        moderate amount of debugging information, or to 2 to get more.} 
    \item{\dots}{optional arguments passed to plotting functions.}
}


\details{In the normal use, \code{drawPalette} draws an image palette near the
    right-hand side of the plotting device, and then adjusts the global margin
    settings in such a way as to cause the next plot to appear (with much
    larger width) to the left of the palette. The function can also be used, if
    \code{zlim} is not provided, to adjust the margin without drawing anything;
    this is useful in lining up the x axes of a stack of plots, some some of
    which will have palettes and others not.

    Note that the plot positioning is done entirely with margins, not with
    \code{par(mfrow)} or other R schemes for multi-panel plots.  This means
    that the user is free to use those schemes without worrying about nesting
    or conflicts; see see \dQuote{Examples}.

}

\value{None.}

\seealso{This is used by \code{\link{imagep}}. }

\examples{
    library(oce)
    par(mgp=getOption("oceMgp"))
    par(mar=c(3,3,1,1))
    par(mfrow=c(4,1))

    drawPalette(zlim=c(0,1),col=oceColorsJet(5))
    plot(1:5, 1:5, col=oceColorsJet(5)[1:5],pch=20,cex=5,xlab='x', ylab='y')

    ## put space for palette to right of next graph (so x scales line up in stacked plot)
    drawPalette()
    plot(1:10, 1:10, col=oceColorsPalette(10)[1:10],pch=20,cex=5,xlab='x', ylab='y')

    drawPalette(zlim=c(0,1),zlab="Image Scale",col=oceColorsPalette)
    plot(1:10, 1:10, lwd=2, bg=oceColorsPalette(10)[1:10], col='black',xlab='x',ylab='y')
    drawPalette(zlab="Image Scale")
    plot(1:10, 1:10, col=oceColorsPalette(10)[1:10], xlab='x', ylab='y')

    # Use layout to mimic the action of imagep(), with the width
    # of the palette region being 14 percent of figure width.
    d <- 0.14
    layout(matrix(1:2,nrow=1), widths=c(1-d,d))
    image(volcano, col=oceColorsJet(100), zlim=c(90, 200))
    contour(volcano, add=TRUE)
    drawPalette(c(90, 200), fullpage=TRUE, col=oceColorsJet)
}

\author{Dan Kelley, with help from Clark Richards}

\keyword{misc}
