% vim:spell:textwidth=100:expandtab:shiftwidth=2:softtabstop=2
\name{webtide}

\alias{webtide}

\title{Get a tidal prediction from a WebTide database}

\description{Get a tidal prediction from a WebTide database}

\usage{webtide(action=c("map", "predict"),
        node, longitude, latitude, time,
        basedir="/usr/local/WebTide", region="nwatl", plot=TRUE,
        tformat, \dots)}

\arguments{

  \item{action}{An indication of the action, either \code{map} to draw a map and measure the
    coordinates of a point the user clicks on that map, or \code{predict} to predict time-series for
    a specific location.}
    
  \item{node}{node to look up; only needed if \code{longitude} and \code{latitude}
   are not given.}

  \item{longitude}{longitude at which prediction is required (ignored if \code{node} is given).}

  \item{latitude}{latitude at which prediction is required (ignored if \code{node} is given).}

  \item{time}{times at which prediction is to be made.}

  \item{basedir}{direction in which the WebTide databases are installed.}

  \item{region}{database region, given as a directory name in the WebTide directory.  For example,
    \code{h3o} is for Halifax Harbour, \code{nwatl} is for the northwest Atlantic, and \code{sshelf}
    is for the Scotian Shelf and Gulf of Maine.}
  \item{plot}{boolean indicating whether to plot.}
  \item{tformat}{optional argument passed to \code{\link{oce.plot.ts}}, for
      plot types that call that function.  (See \code{\link{strptime}} for the
      format used.)}
  \item{\dots}{optional arguments passed to plotting functions. A common
    example is to set \code{xlim} and \code{ylim}, to focus a map region.}
}

\details{The \code{action="map"} method plots a map with a dot for the lower-left corner of each
  triangle used in the finite-element tidal simulation upon which WebTide predictions are based.
  \code{\link{locator}} is called within this method, so that the user can indicate a spot of
  interest on the map, and the longitude and latitude of this point are provided in the return value.
  This is a crude emulation of the WebTide interface, without the ability to zoom or slide reference
  points around with the mouse.  The \code{action="predict"} method uses a longitude and latitude,
  possibly discovered from clicking the mouse, and calculates elevation and velocity time-series at
  the specified times.  In the first case, a plot is drawn irrespective of the value of \code{plot},
  but in the second case, a plot is only made if \code{plot=TRUE}.  Naturally, none of this works
  unless WebTide has been installed on the computer.}

\value{If \code{action="map"} and \code{plot=TRUE}, the return value is a list containing the index
  of the nearest node, along with the \code{latitude} and \code{longitude} of that node.  If
  \code{action="map"} and \code{plot=FALSE},  the return value is a list of all nodes, longitude,
  and latitudes.  
  
  If \code{action="predict"}, the return value is a list containing a vector of times (\code{time}),
  as well as vectors of the predicted \code{elevation} in metres and the predicted horizontal
  components of velocity, \code{u} and \code{v}, along with the \code{node} number, and the
  \code{basedir} and \code{region} as supplied to this function.}
  
\examples{ \dontrun{ library(oce) tStart <- ISOdate(2008, 7, 1, 0, 0, 0, tz="UTC") tEnd <-
ISOdate(2008, 7, 5, 0, 0, 0, tz="UTC") time<-seq(tStart, tEnd, by=15, units="minutes") prediction <-
  webtide("predict", -69.61, 48.14, time) } }

\references{The WebTide software may be downloaded for free at
  \url{http://www.bio.gc.ca/research-recherche/WebTide-MareeWeb/webtide-eng.htm}, along with a suite
  of databases for various geographical regions.  Note that WebTide is not an open-source
  application, as it consists mainly of precompiled code written in Java, which precludes
  examination of the source.}

\author{Dan Kelley}

\keyword{misc}

\concept{tide}

