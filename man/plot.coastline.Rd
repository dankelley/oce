\name{plot.coastline}

\alias{plot.coastline}

\title{Plot a coastline}

\description{Plot a coastline.}

\usage{\method{plot}{coastline}(x, asp=NA, mgp=getOption("oce.mgp"), bg, axes=TRUE, \dots)}

\arguments{
  \item{x}{A \code{coastline} object, e.g. as read by
    \code{\link{read.coastline}} or created by
    \code{\link{as.coastline}}.}
  \item{asp}{Aspect ratio for plot.  The default is for
    \code{plot.coastline} to set the aspect ratio to give natural
    latitude-longitude scaling at the average latitude in the data range.
    In most cases, you will probably want to set \code{asp} yourself,
    e.g. coastal shapes near 45N will have the correct shape if you set
    \code{asp=1/cos(45*pi/180)}.  Setting \code{asp=1} makes sense for
    world views.}
  \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
    also for \code{par(mar)}, computed from this.  The default is
    tighter than the R default, in order to use more space for the
    data and less for the axes.}
  \item{bg}{optional colour to be used for the background of the map.
    This comes in handy for drawing insets (see \dQuote{details}).}
  \item{axes}{boolean, set to \code{TRUE} to plot axes.}
  \item{\dots}{optional arguments passed to plotting functions.  For
    example, set \code{yaxp=c(-90,90,4)} for a plot extending from pole to
    pole.}
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
data(coastline.world)
plot(coastline.world)
}

\author{Dan Kelley}

\keyword{misc}
