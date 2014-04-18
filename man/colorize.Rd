\name{colorize}

\alias{colorize}

\title{Calculate colors (DRAFT VERSION)}

\description{Calculate colors for use in palettes and plot}

\usage{colorize(z, breaks, colors=oceColorsJet, palette, breaksPerLevel=1)}

\arguments{

  \item{z}{a vector or other set of numerical values to be examined.}

  \item{breaks}{a vector of numerical values for break-points between color
      levels.  If this is not provided, and if \code{palette} is missing, then
      \code{pretty(z,n=10)} is used.  If \code{breaks} is provided but consists
      of only a single number, then again \code{\link{pretty}} is called, but
      with \code{n} set to that number.}
  
  \item{colors}{a function that takes a numerical value as its single argument,
      and that returns a vector of colours.  Ignored if \code{palette} is
      provided.}

  \item{palette}{a named palette.  If provided, \code{breaks} and \code{colors}
      are ignored, and the indicated palette is used.  Presently the only
      choices for \code{palette} are \code{"GMT_relief"}, \code{"GMT_ocean"}
      and \code{"globe"}. See \sQuote{References} for more about these
      GMT-based schemes.}
  
  \item{breaksPerLevel}{number of breaks per level; only used if \code{palette}
      is given.}
}

\details{This function may be used to calculate \code{breaks} and \code{col}
    for \code{\link{image}}, \code{\link{imagep}} or \code{\link{drawPalette}},
    at the same time as calculating \code{col} that might be used with
    \code{\link{plot}}.  Using this function together with
    \code{\link{drawPalette}} and \code{\link{plot}} provides an assurance of
    color matching between the palette and scatter-plot.

    There are two modes of use.  If \code{palette} is not supplied, this
    function uses \code{breaks} and/or \code{colors} to construct a color
    scheme.  However, if \code{palette} is supplied, it takes precedence over
    \code{breaks} and \code{colors}.

    Palettes are convenient because they provide standardized linkage between
    value and colour, which is helpful when certain values are naturally linked
    with certain colours, e.g. zero elevation or current velocity.}

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

## 1. scatterplot
mar <- par('mar') # prevent drawPalette() causing margin creep
cs <- colorize(y)
drawPalette(cs$zlim, col=cs$col, breaks=cs$breaks)
plot(x, y, bg=cs$zcol, pch=21, cex=1)
grid()
par(mar=mar)

## 2. imagep()
data(topoWorld)
par(mfrow=c(2, 1))
pal <- colorize(palette="GMT_relief")
imagep(topoWorld, breaks=pal$breaks, col=pal$col, asp=1)
pal <- colorize(palette="globe")
imagep(topoWorld, breaks=pal$breaks, col=pal$col, asp=1)
}

\references{Information on GMT software is given at
    \url{http://gmt.soest.hawaii.edu}.  Diagrams showing the GMT color schemes
    are at \url{http://www.geos.ed.ac.uk/it/howto/GMT/CPT/palettes.html}, and
    their numerical specifications are at
    \url{http://www.beamreach.org/maps/gmt/share/cpt}.}

\author{Dan Kelley}

\keyword{misc}

