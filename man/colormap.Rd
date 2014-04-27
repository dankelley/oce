\name{colormap}

\alias{colormap}

\title{Calculate colors (Draft Version)}

\description{Calculate colors for use in palettes and plots}

\usage{colormap(name, x0, x1, col0, col1, n=1)}

\arguments{

  \item{name}{an optional character string naming a built-in colormap (one of
      \code{"gmt_relief"}, \code{"gmt_ocean"}, \code{"gmt_globe"} or
      \code{"gmt_gebco"}) or a filename containing information describing the
      colormap in GMT format, e.g.
      \code{"http://www.beamreach.org/maps/gmt/share/cpt/GMT_globe.cpt"}). If
      \code{name} is provided, then all other arguments to the function are
      ignored.}

  \item{x0, x1, col0, col1}{Vectors that specify the colormap.  \code{x0}
      and \code{x1} are numerical values, while \code{col0} and \code{col1}
      are colors, either in the form of naming strings color such as
      \code{"red"} or in the form of the values returned by \code{\link{rgb}}
      or \code{\link{hsv}}.  }
  
  \item{n}{number of breaks per (\code{x0}, \code{x1}) pair, used to create a
      colormap palette from just a few levels and colors.}

} 

\details{This function may be called by \code{\link{colorize}} in response to
    its \code{colormap} argument, and may also be called directly by the user
    (see \dQuote{Examples}).  There are two methods for use.

    Method 1. If \code{name} is given, a color map is selected from a list of
    built-in choices, or scanned from a file in GMT format.  As illustrated in
    \dQuote{Examples}, this color map may be modified before being passed to
    \code{\link{colorize}}, and this is perhaps the easiest way to create
    customized color maps.
    
    Method 2. If \code{name} is not given, a colormap is constructed by
    interpolating within \code{x0}, \code{x1}, etc.  This is done pair-wise
    within each vector, e.g. \code{\link{seq}} is used to create \code{n}
    output values between \code{x0[1]} and \code{x0[2]}, then between
    \code{x0[2]} and \code{x0[3]}, etc.  The same is done for \code{x1} and for
    the colors \code{col0} and \code{col1}.  In this way, a colormap can be
    constructed by specifying a few points.}

\value{A list (containing \code{x0}, \code{x1}, \code{col0}, \code{col1}, and
    \code{missingColor}) suitable as the \code{colormap} argument of
    \code{\link{colorize}}.}

\section{Development status}{Devised in April 2014, this function is likely to
    be somewhat unstable until mid-summer 2014, with changes to the argument
    list and return value.  The same may be said of its cousin,
    \code{\link{colorize}}.}
    
\examples{
\dontrun{
library(oce)
data(topoWorld)

## Topo map with GMT "globe" scheme
cm <- colormap("gmt_globe")
cz <- colorize(colormap=cm)
imagep(topoWorld, breaks=cz$breaks, col=cz$col)

## Modify to red for depths < 1km
cm <- colormap("gmt_globe")
km <- which(-1000 <= cm$x0 & cm$x0 < 0)
cm$col0[km] <- 'red'
cm$col1[km] <- 'red'
cz <- colorize(colormap=cm)
imagep(topoWorld, breaks=cz$breaks, col=cz$col)

## Construct color map
cm <- colormap(x0=c(-8000, -4000, 0, 100),
               x1=c(-8000, -4000, 0, 100),
               col0=c("violet","blue","white","tan"),
               col1=c("violet","blue","white","tan"),n=10)
cz <- colorize(colormap=cm)
imagep(topoWorld, breaks=cz$breaks, col=cz$col)
}
}

\references{Information on GMT software is given at
    \url{http://gmt.soest.hawaii.edu}.  Diagrams showing the GMT color schemes
    are at \url{http://www.geos.ed.ac.uk/it/howto/GMT/CPT/palettes.html}, and
    their numerical specifications are at e.g.
    \url{http://www.beamreach.org/maps/gmt/share/cpt}.}

\author{Dan Kelley}

\keyword{misc}

