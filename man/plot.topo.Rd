\name{plot.topo}

\alias{plot.topo}

\title{Plot topography data}

\description{Plot contours of topographic data.}

\usage{\method{plot}{topo}(x, 
     xlab="", ylab="",
     asp,
     center, span,
     expand=1.5,
     water.z,
     water.col,
     water.lty,
     water.lwd,
     land.z, 
     land.col,
     land.lty,
     land.lwd,
     legend.loc="topright", 
     mgp=getOption("oce.mgp"),
     mar=c(mgp[1]+1,mgp[1]+1,1,1),
     debug=getOption("oce.debug"),
     \dots)}

\arguments{
  \item{x}{an \code{topo} object, e.g. as read by \code{\link{read.topo}}.}
  \item{xlab}{label for x axis}
  \item{ylab}{label for y axis}
  \item{asp}{Aspect ratio for plot.  The default is for
    \code{plot.coastline} to set the aspect ratio to give natural
    latitude-longitude scaling somewhere near the centre latitude on the
    plot. Often, it makes sense to set \code{asp} yourself, e.g. to get
    correct shapes at 45N, use \code{asp=1/cos(45*pi/180)}. 
    Note that the land mass is not symmetric about the equator, so
    to get good world views you should set \code{asp=1} or 
    set \code{ylim} to be symmetric about zero.  Any given value of
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
  \item{water.z}{depths at which to plot water contours.  If not
    provided, these are inferred from the data.}
  \item{water.col}{colors corresponding to \code{water.z} values.  If
    not provided, these will be \code{"fill"} colors from
    \code{\link{oce.colors.gebco}}.}
  \item{water.lty}{line type(s) for water contours}
  \item{water.lwd}{line width(s) for water contours}
  \item{land.z}{depths at which to plot land contours.  If not provided,
    these are inferred from the data.  If set to \code{NULL}, no land
    contours will be plotted.}
  \item{land.col}{colors corresponding to \code{land.z} values.  If
    not provided, these will be \code{"fill"} colors from
    \code{\link{oce.colors.gebco}}.}
  \item{land.lty}{line type(s) for land contours}
  \item{land.lwd}{line width(s) for land contours}
  \item{legend.loc}{location for a legend (or FALSE, for no legend).}
  \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
    also for \code{par(mar)}, computed from this.  The default is
    tighter than the R default, in order to use more space for the data
    and less for the axes.}
  \item{mar}{value to be used with \code{\link{par}("mar")}.}
  \item{debug}{boolean value, set to \code{TRUE} to get some debugging
    information.}
  \item{\dots}{additional arguments passed on to plotting functions}
}

\details{The plot aspect ratio is set based on the middle latitude in
  the plot.  Be aware that the ETOPO2 data are on a 2-minute (roughly
  2-mile) spacing, and coastlines created with such data can be quite
  inaccurate on scales of 100km or less.

  The line properties, such as \code{land.lwd}, may either be a single
  item, or a vector; in the latter case, the length must match the
  length of the corresponding properties, e.g. \code{land.z}.
}

\seealso{TOPO data can be created with \code{\link{read.topo}} and
  summarized with \code{\link{summary.topo}}.
}

\examples{
library(oce)
data(topo.maritimes)
plot(topo.maritimes)
}

\author{Dan Kelley}

\keyword{misc}
