\name{colorize}

\alias{colorize}

\title{Calculate colors (Draft Version)}

\description{Calculate colors for use in palettes and plots.}

\usage{colorize(z, breaks, col=oceColorsJet, colormap, segments=1, missingColor="gray")}

\arguments{

  \item{z}{an optional vector or other set of numerical values to be examined.}

  \item{breaks}{an optional vector of numerical values for break points between
      color levels (see \code{\link{image}}).  If this is not provided, and if
      \code{colormap} is missing, then \code{pretty(z,n=10)} is used.  If
      \code{breaks} is provided but consists of only a single number, then
      again \code{\link{pretty}} is called, but with \code{n} set to that
      number.}
  
  \item{col}{either a vector of colors or a function taking a numerical value
      as its single argument and returning a vector of colors (see
      \code{\link{image}}.  The value of \code{col} is ignored if
      \code{palette} is provided.}

  \item{colormap}{optional specification of a color map.  This may be a
      character string naming a built-in colormap (\code{"gmt_relief"},
      \code{"gmt_ocean"}, \code{"gmt_globe"} or \code{"gmt_gebco"}), or naming
      a file that contains a colormap in GMT format (see \dQuote{References}).
      Alternatively, it may be the output from \code{\link{colormap}}.  If
      \code{colormap} is given, then \code{breaks} and \code{col} are ignored.}
  
  \item{segments}{number of segments between any two color levels, used only if
      \code{colormap} is given.  The default value of 1 yields uniform color
      between \code{x0[1]} and \code{x0[2]}, etc.  while higher values yield
      substeps.  This is a way to construct a color scheme from a few
      constituents, as an alternative to specifying \code{segments} in a call
      to \code{\link{colormap}}.}

  \item{missingColor}{colour to use for missing values.  This will be set
      automatically if \code{colormap} is given.}
}

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

\value{A list containing \code{zlim}, \code{breaks}, \code{col}, \code{zcol}
    and \code{missingColor}.  See \sQuote{Examples} for how these may be used
    in plotting.}

\section{Development status}{Devised in April 2014, this function is likely to
    be somewhat unstable until mid-summer 2014, with changes to the argument
    list and return value.  The same may be said of its cousin,
    \code{\link{colormap}}.}

\examples{
library(oce)

## 1. colorize points on xy plot
x <- seq(0, 1, length.out=40)
y <- sin(2 * pi * x)
mar <- par('mar') # prevent margin creep by drawPalette()
c <- colorize(y)
drawPalette(c$zlim, col=c$col, breaks=c$breaks)
plot(x, y, bg=c$zcol, pch=21, cex=1)
grid()
par(mar=mar)

## 2. colorize image of N. Atl. topography
\dontrun{
data(topoWorld)
natl <- subset(subset(topoWorld, longitude > 270), latitude > 0)
c <- colorize(colormap="gmt_relief")
imagep(natl, breaks=c$breaks, col=c$col)
}
}

\references{Information on GMT software is given at
    \url{http://gmt.soest.hawaii.edu}.  Diagrams showing the GMT color schemes
    are at \url{http://www.geos.ed.ac.uk/it/howto/GMT/CPT/palettes.html}, and
    their numerical specifications are at e.g.
    \url{http://www.beamreach.org/maps/gmt/share/cpt}.}

\author{Dan Kelley}

\keyword{misc}

