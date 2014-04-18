\name{colorize}

\alias{colorize}

\title{Calculate colors (DRAFT VERSION)}

\description{Calculate colors for use in palettes and plot}

\usage{
colorize(z, breaks, colors=oceColorsJet)
}

\arguments{

  \item{z}{a vector or other set of numerical values to be examined.}

  \item{breaks}{a vector of numerical values for break-points between color
      levels.  If this is not provided, \code{pretty(z),n=10} is used.  If
      \code{breaks} is provided but consists of only a single number, then
      again \code{\link{pretty}} is called, but with \code{n} set to that
      number.}
  
  \item{colors}{a function that takes a numerical value as its single argument,
      and that returns a vector of colours.}

}

\details{This function is in a preliminary form (for testing), and its
    arguments and action may change through the Summer of 2014.}

\value{A list containing \code{breaks} and \code{col}, as used by e.g.
    \code{\link{image}} or \code{\link{imagep}}.}

\section{Development status}{Devised in April 2014, this function is likely to
    be somewhat unstable until mid-summer 2014.  In particular, the argument
    list may change, becuase there is a possibility of combining this with the
    existing (and again, preliminary) function \code{\link{makePalette}}.}

\examples{
library(oce)
x <- seq(0, 1, length.out=40)
y <- sin(2 * pi * x)

par(mfrow=c(2, 1), mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
mar <- par('mar') # prevent drawPalette() causing margin creep
cs <- colorize(y)
drawPalette(cs$zlim, col=cs$col, breaks=cs$breaks)
plot(x, y, bg=cs$zcol, pch=21, cex=1)
grid()
par(mar=mar)

cs <- colorize(y, col=oceColorsPalette)
drawPalette(cs$zlim, col=cs$col, breaks=cs$breaks)
plot(x, y, bg=cs$zcol, pch=21, cex=1)
grid()

}

\references{The GMT documentation is at (\url{http://gmt.soest.hawaii.edu}, and
    files suitable for the use here are at e.g.
    \url{http://www.beamreach.org/maps/gmt/share/cpt}.}

\author{Dan Kelley}

\keyword{misc}

