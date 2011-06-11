\name{plot.coastline}

\alias{plot.coastline}

\title{Plot a coastline}

\description{Plot a coastline.}

\usage{\method{plot}{coastline}(x,
     xlab="", ylab="",
     asp,
     center, span,
     expand=1.5,
     mgp=getOption("oceMgp"), 
     mar=c(mgp[1]+1,mgp[1]+1,1,1),
     bg,
     fill='lightgray',
     axes=TRUE,
     debug=getOption("oceDebug"),
     \dots)}

\arguments{
  \item{x}{A \code{coastline} object, as read by
    \code{\link{read.coastline}} or created by
    \code{\link{as.coastline}}, or a list containing items named
    \code{latitude} and \code{longitude}.} 
  \item{xlab}{label for x axis}
  \item{ylab}{label for y axis}
  \item{asp}{Aspect ratio for plot.  The default is for
    \code{plot.coastline} to set the aspect ratio to give natural
    latitude-longitude scaling somewhere near the centre latitude on the
    plot. Often, it makes sense to set \code{asp} yourself, e.g. to get
    correct shapes at 45N, use \code{asp=1/cos(45*pi/180)}. 
    Note that the land mass is not symmetric about the equator, so
    to get good world views you should set \code{asp=1} or 
    set \code{ylim} to be symmetric about zero. Any given value of
    \code{asp} is ignored, if \code{center} is given.}
  \item{center}{optional center of map, a vector containing latitude in
    degrees north, then longitude in degrees east. (Must be accompanied
    by \code{span}.)}
  \item{span}{optional suggested span of plot, in kilometers (must be
    supplied, if \code{center} is supplied).}
  \item{expand}{numerical factor for the expansion of plot limits,
    showing area outside the plot, e.g. if showing a ship track as a
    coastline, and then an actual coastline to show the ocean boundary.
    The value of \code{expand} is ignored if either \code{xlim} or
    \code{ylim} is given.}
  \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
    also for \code{par(mar)}, computed from this.  The default is
    tighter than the R default, in order to use more space for the
    data and less for the axes.}
  \item{mar}{value to be used with \code{\link{par}("mar")}.}
  \item{bg}{optional colour to be used for the background of the map.
    This comes in handy for drawing insets (see \dQuote{details}).}
  \item{fill}{colour to be used to fill land regions.  This is ignored
    unless the coastline object is fillable (e.g. for a "shapefile" read
    by \code{\link{read.coastline}}).  Set to \code{NULL} to turn off
    filling.} 
  \item{axes}{boolean, set to \code{TRUE} to plot axes.}
  \item{debug}{set to \code{TRUE} to get debugging information during
    processing.}
  \item{\dots}{optional arguments passed to plotting functions.  For
    example, set \code{yaxp=c(-90,90,4)} for a plot extending from pole
    to pole.}
}

\details{This function plots a coastline.  An attempt is made to fill
  the space of the plot, and this is done by limiting either the
  longitude range or the latitude range, as appropriate, by modifying
  the eastern or northern limit, as appropriate.

  To get an inset map inside another map, draw the first map, do
  \code{par(new=TRUE)}, and then call \code{plot.coastline} with a value
  of \code{mar} that moves the inset plot to a desired location on the
  existing plot, and with \code{bg="white"}.}

\value{None.}

\seealso{\code{\link{read.coastline}} can read coastlines from data
  files, provided that they are in a certain class of formats.  A
  coastline may be summarized by \code{\link{summary.coastline}}.}

\examples{
library(oce)
data(coastlineWorld)
plot(coastlineWorld)
}

\author{Dan Kelley}

\keyword{hplot}
