% vim:textwidth=100:expandtab:shiftwidth=2:softtabstop=2
\name{webtide}

\alias{webtide}

\title{Get a tidal prediction from a WebTide database}

\description{Get a tidal prediction from a WebTide database}

\usage{webtide(action=c("map", "predict"), latitude, longitude, time,
       basedir="~/WebTide", region="nwatl", plot=TRUE)}

\arguments{

  \item{action}{An indication of the action, either \code{map} to draw a map and measure the
    coordinates of a point the user clicks on that map, or \code{predict} to predict time-series
    for a specific location.}
    
  \item{latitude}{latitude at which prediction is required.}

  \item{longitude}{longitude at which prediction is required.}

  \item{time}{times at which prediction is to be made.}

  \item{basedir}{direction in which the WebTide databases are installed.}

  \item{region}{database region, given as a directory name in the WebTide directory.  For example,
    \code{h3o} is for Halifax Harbour, \code{nwatl} is for the northwest Atlantic, and \code{sshelf}
    is for the Scotian Shelf and Gulf of Maine.}

  \item{plot}{boolean indicating whether to plot.}
  
}

\details{The \code{action="map"} method plots a map, upon which the user should click the mouse; it
  returns a list of the latitude and the longitide.  The \code{action="predict"} method uses a
  latitude and longitude, possibly discovered from clicking the mouse, and calculates elevation and
  velocity time-series at the specified times.  In the first case, a plot is drawn irrespective of
  the value of \code{plot}, but in the second case, a plot is only made if \code{plot=TRUE}.}

\examples{
library(oce)
point <- webtide("map", region="h3o")
tStart <- ISOdate(2011, 11, 7, 0, 0, 0, tz="UTC")
tEnd <- ISOdate(2011, 11, 10, 0, 0, 0, tz="UTC")
time<-seq(tStart, tEnd, by=15, units="minutes")
prediction <- webtide("predict", point$latitude, point$longitude, time, region="h3o")

# SLEIWEX region
tStart <- ISOdate(2008, 7, 1, 0, 0, 0, tz="UTC")
tEnd <- ISOdate(2008, 7, 5, 0, 0, 0, tz="UTC")
time<-seq(tStart, tEnd, by=15, units="minutes")
prediction <- webtide("predict", 48.14, -69.61, time, region="nwatl")

}

\references{The WebTide databases are available at
  \url{http://www.bio.gc.ca/research-recherche/WebTide-MareeWeb/webtide-eng.htm} on the web.}

\author{Dan Kelley}

\keyword{misc}

\concept{tide}

