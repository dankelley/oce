\name{drawPalette}

\alias{drawPalette}

\title{Draw a palette on the right-hand side of plot device}

\description{Draw a palette on the right-hand side of plot device}

\usage{drawPalette(zlim, zlab="", breaks, col, top=0, bottom=0, drawContours=FALSE, debug=getOption("oceDebug"), \dots)}

\arguments{
    \item{zlim}{two-element vector containing the lower and upper limits of z.
        If not provided, the palette is not drawn, but space is set aside for it,
        so that the next plotting command has a right-hand margin matching the
        space that would have been occupied by the palette.}
    \item{zlab}{label for the palette scale.}
    \item{breaks}{the z values for breaks in the colour scheme.}
    \item{col}{either a vector of colours corresponding to the breaks, of length
        1 plus the number of breaks, or a function specifying colours,
        e.g. \code{\link{oceColorsJet}} for a rainbow.}
    \item{top}{extra space to insert at top of palette, in inches}
    \item{bottom}{extra space to insert at bottom of palette, in inches}
    \item{drawContours}{boolean, \code{TRUE} to get contours on the
        palette, at the colour breaks.}
    \item{debug}{a flag that turns on debugging.  Set to 1 to get a
        moderate amount of debugging information, or to 2 to get more.} 
    \item{\dots}{optional arguments passed to plotting functions.}
}

\details{

    \emph{This is an early version, which may be removed if it does not prove
        useful.  The calling arguments may be changed also.}

    In the normal use, \code{drawPalette} draws an image palette near the
    right-hand side of the plotting device, and then adjusts the global margin
    settings in such a way as to cause the next plot to appear to the left of
    the palette. The function can also be used, if \code{zlim} is not provided,
    to adjust the margin without drawing anything; this is useful in lining up
    the x axes of a stack of plots, some some of which will have palettes and
    others not.
    
    Note that the plot positioning is done entirely with margins, not with
    \code{par(mfrow)} or other R schemes for multi-panel plots.  This means
    that the user is free to use those schemes without worrying about nesting
    or conflicts; see see \dQuote{Examples}.

}

\value{None.}

\seealso{
    This may eventually be used by \code{\link{imagep}}.
}
\examples{
par(mgp=getOption("oceMgp"))
par(mar=c(3,3,1,1))
par(mfrow=c(4,1))

drawPalette(zlim=c(0,1),col=oceColorsJet(5))
plot(1:5, 1:5, col=oceColorsJet(5)[1:5],pch=20,cex=5,xlab='x', ylab='y', main='test 1')

## put space for palette to right of next graph (so x scales line up in stacked plot)
drawPalette()
plot(1:10, 1:10, col=oceColorsPalette(10)[1:10],pch=20,cex=5,xlab='x', ylab='y', main='test 2')

drawPalette(zlim=c(0,1),zlab="Image Scale",col=oceColorsPalette)
plot(1:10, 1:10, lwd=2, bg=oceColorsPalette(10)[1:10],pch=21,cex=5,col='black',xlab='x', ylab='y', main='test 3')
drawPalette(zlab="Image Scale")
plot(1:10, 1:10, col=oceColorsPalette(10)[1:10],pch=20,cex=5,xlab='x', ylab='y', main='test 4')

}
\author{Dan Kelley}
\keyword{misc}
