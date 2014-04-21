\name{colorize}

\alias{colorize}

\title{Calculate colors (DRAFT VERSION)}

\description{Calculate colors for use in palettes and plots}

\usage{colorize(z, breaks, colors=oceColorsJet, colormap, breaksPerLevel=1)}

\arguments{

  \item{z}{a vector or other set of numerical values to be examined.}

  \item{breaks}{optional vector of numerical values for break points between
      color levels.  If this is not provided, and if \code{colormap} is
      missing, then \code{pretty(z,n=10)} is used.  If \code{breaks} is
      provided but consists of only a single number, then again
      \code{\link{pretty}} is called, but with \code{n} set to that number.}
  
  \item{colors}{either a vector of colors or a function taking a numerical
      value as its single argument and returning a vector of colours.  The
      value of \code{colors} is ignored if Ignored if \code{palette} is
      provided.}

  \item{colormap}{a character string naming a built-in colormap, or a colormap
      that was created with \code{\link{colormap}}.  If \code{colormap} is
      given, then it takes precedence over both \code{breaks} and
      \code{colors}.  The only built-in choices are \code{"GMT_relief"},
      \code{"GMT_ocean"} and \code{"globe"}.  See \sQuote{References} for more
      about these GMT-based schemes.}
  
  \item{breaksPerLevel}{number of breaks per level; only used if
      \code{colormap} is given.  If \code{breaksPerLevel} is a large enough
      number (say 10 or more) then the colors will blend smoothly between the
      discrete values.} }

\details{This function may be used to calculate \code{breaks} and \code{col}
    for \code{\link{image}}, \code{\link{imagep}} or \code{\link{drawPalette}},
    at the same time as calculating \code{col} that might be used with
    \code{\link{plot}}.  Using this function together with
    \code{\link{drawPalette}} and \code{\link{plot}} provides an assurance of
    color matching between the palette and scatter-plot.

    There are two modes of use.  If \code{colormap} is not supplied, this
    function uses \code{breaks} and/or \code{colors} to construct a color
    scheme; the meaning of these values is the same as for \code{\link{image}}.

    However, if \code{colormap} is supplied, it takes precedence over
    \code{breaks} and \code{colors}.  Colormaps can be convenient because they
    provide standardized linkage between value and colour, which is helpful
    when certain values are naturally linked with certain colours, e.g. zero
    elevation or current velocity.}

\value{A list containing \code{breaks} and \code{col}, as used by e.g.
    \code{\link{image}} or \code{\link{imagep}}.}

\section{Development status}{Devised in April 2014, this function is likely to
    be somewhat unstable until mid-summer 2014.  In particular, the argument
    list may change, becuase there is a possibility of combining this with the
    existing (and again, preliminary) function \code{\link{colormap}}.}

\examples{
library(oce)
x <- seq(0, 1, length.out=40)
y <- sin(2 * pi * x)

## 1. scatterplot
mar <- par('mar') # prevent margin creep by drawPalette()
c <- colorize(y)
drawPalette(c$zlim, col=c$col, breaks=c$breaks)
plot(x, y, bg=c$zcol, pch=21, cex=1)
grid()
par(mar=mar)

\dontrun{
## 2. imagep()
data(topoWorld)
cm <- colormap(palette="GMT_relief")
imagep(topoWorld, breaks=cm$breaks, col=cm$col)
}
}

\references{Information on GMT software is given at
    \url{http://gmt.soest.hawaii.edu}.  Diagrams showing the GMT color schemes
    are at \url{http://www.geos.ed.ac.uk/it/howto/GMT/CPT/palettes.html}, and
    their numerical specifications are at
    \url{http://www.beamreach.org/maps/gmt/share/cpt}.}

\author{Dan Kelley}

\keyword{misc}

