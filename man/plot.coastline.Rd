\name{plot.coastline}

\alias{plot.coastline}
\alias{plot,coastline,missing-method}
\alias{plot,coastline-method}

\title{Plot a coastline}

\description{Plot a coastline}

\usage{\S4method{plot}{coastline}(x,
     xlab="", ylab="",
     asp,
     clatitude, clongitude, span,
     expand=1,
     mgp=getOption("oceMgp"), 
     mar=c(mgp[1]+1,mgp[1]+1,1,1),
     bg,
     fill='lightgray',
     axes=TRUE, cex.axis=par('cex.axis'),
     add=FALSE, inset=FALSE,
     geographical=FALSE,
     debug=getOption("oceDebug"),
     \dots)}

\arguments{
  \item{x}{A \code{coastline} object, as read by
    \code{\link{read.coastline}} or created by
    \code{\link{as.coastline}}, or a list containing items named
    \code{latitude} and \code{longitude}.} 
  \item{xlab}{label for x axis}
  \item{ylab}{label for y axis}
  \item{asp}{Aspect ratio for plot.  The default is for \code{plot.coastline}
      to set the aspect ratio to give natural latitude-longitude scaling
      somewhere near the centre latitude on the plot. Often, it makes sense to
      set \code{asp} yourself, e.g. to get correct shapes at 45N, use
      \code{asp=1/cos(45*pi/180)}.  Note that the land mass is not symmetric
      about the equator, so to get good world views you should set \code{asp=1}
      or set \code{ylim} to be symmetric about zero. Any given value of
      \code{asp} is ignored, if \code{clatitude} and \code{clongitude} are
      given.}
  \item{clatitude}{optional center latitude of map, in degrees north.  If this
      and \code{clongitude} are provided, then any provided value of \code{asp}
      is ignored, and instead the plot aspect ratio is computed based on the
      center latitude.  Also, if \code{clatitude} and \code{clongitude} are
      provided, then \code{span} must be, also.}
  \item{clongitude}{optional center longitude of map, in degrees east; see
      \code{clatitude}.}
  \item{span}{optional suggested span of plot, in kilometers.  The suggestion
      is an upper limit on the scale; depending on the aspect ratio of the
      plotting device, the radius may be smaller than \code{span}.  A value for
      \code{span} must be supplied, if \code{clatitude} and \code{clongitude}
      are supplied.}
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
  \item{cex.axis}{value for axis font size factor.}
  \item{add}{boolean, set to \code{TRUE} to draw the coastline on an existing
      plot.  Note that this retains the aspect ratio of that existing plot, so
      it is important to set that correctly, e.g. with \code{asp=1/cos(lat * pi
          / 180)}, where \code{clat} is the central latitude of the plot.}
  \item{inset}{set to \code{TRUE} for use within \code{\link{plotInset}}.  The
      effect is to prevent the present function from adjusting margins, which
      is necessary because margin adjustment is the basis for the method
      used by \code{\link{plotInset}}.}
  \item{geographical}{set to \code{TRUE} to get latitude and longitude labels
      that are always positive.}
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

\seealso{The documentation for \code{\link{coastline-class}} explains the
    structure of coastline objects, and also outlines the other functions
    dealing with them.}

\examples{
library(oce)
data(coastlineWorld)
plot(coastlineWorld)
plot(coastlineWorld, clatitude=44.6, clongitude=-63.6, span=1000)
# Test code, examplifying how a new option could yield geographical labels
data(coastlineMaritimes)
plot(coastlineMaritimes, axes=FALSE, span=300)
x <- pretty(par('usr')[1:2], n=1.2*floor(par('fin')[1])) # the 1.2 bears some thinking; should cin be used?
y <- pretty(par('usr')[3:4], n=1.2*floor(par('fin')[2]))
box()
axis(side=1,at=x,labels=formatPosition(x,type='expression'))
axis(side=2,at=y,labels=formatPosition(y,type='expression'))
}

\author{Dan Kelley}

\keyword{hplot}
