\name{colormapAlpha}

\alias{colormap}
\alias{colormapAlpha}

\title{Calculate colors (Draft Alpha Version)}

\description{Calculate colors for use in palettes and plots}

\usage{colormapAlpha(name, file, x0, x1, col0, col1, n=1)}

\arguments{

  \item{name}{an optional character string naming a built-in colormap (one of
      \code{"gmt_relief"}, \code{"gmt_ocean"}, or \code{"gmt_globe"}).  If
      \code{name} is provided, all other other arguments to the function are
      ignored.}

  \item{file}{an optional character string naming a file containing a
      GMT-formatted colormap, often a URL (e.g.
      \code{"http://www.beamreach.org/maps/gmt/share/cpt/GMT_globe.cpt"}).  If
      \code{file} is provided (and \code{name} is not), then all other
      arguments to the function are ignored.}

  \item{x0, x1, col0, col1}{Vectors that specify the colormap.  \code{x0}
      and \code{x1} are numerical values, while \code{col0} and \code{col1}
      are colors, either in the form of naming strings color such as
      \code{"red"} or in the form of the values returned by \code{\link{rgb}}
      or \code{\link{hsv}}.  }
  
  \item{n}{number of breaks per (\code{x0}, \code{x1}) pair; see previous
      item.}

} 

\details{This function may be used to calculate \code{breaks} and \code{col}
    for \code{\link{image}}, \code{\link{imagep}} or \code{\link{drawPalette}}.

    The colormap is constructed by examining each (\code{x0}, \code{x1})
    pair in turn. For each such pair, \code{\link{colorRampPalette}} is used to
    interpolate to get \code{n} colors between \code{col0} and \code{col1}.
    From these, a conventional R color scheme is devised and returned in the
    values \code{breaks} and \code{col}.  Setting \code{n} to 1 yields a
    quantized palette.}

\value{A list containing \code{x0}, \code{x1}, \code{col0}, and \code{col1}.
    Typically this is used as the value of the \code{colormap} argument of
    \code{\link{colorize}}, which in turn is used to calculate the conventional
    arguments \code{breaks} and \code{col}, as used by R functions such as
    \code{\link{image}}.  The class of the return value is \code{c("list",
        "colormap")}, although this fact is not used at the present version of
    Oce.}

\section{Development status}{Devised in April 2014, this function is likely to
    be somewhat unstable until mid-summer 2014.  In particular, the argument
    list may change.  The suffix \code{Alpha} is a signal that the function may
    change as it is used in practice; it will be dropped when the function is
    ready for broad use.}
    
\examples{
library(oce)
data(topoWorld)
url <- "http://www.beamreach.org/maps/gmt/share/cpt/GMT_relief.cpt"
cm <- colorizeAlpha(colormap=colormapAlpha(file=url))
imagep(topoWorld, breaks=cm$breaks, col=cm$col)
}

\references{Information on GMT software is given at
    \url{http://gmt.soest.hawaii.edu}.  Diagrams showing the GMT color schemes
    are at \url{http://www.geos.ed.ac.uk/it/howto/GMT/CPT/palettes.html}, and
    their numerical specifications are at e.g.
    \url{http://www.beamreach.org/maps/gmt/share/cpt}.}

\author{Dan Kelley}

\keyword{misc}

