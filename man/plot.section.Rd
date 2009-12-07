\name{plot.section}

\alias{plot.section}

\title{Plot a CTD section}

\description{Plot a CTD section.}

\usage{\method{plot}{section}(x,
     which=1:4,
     at=NULL,
     labels=TRUE,
     grid=FALSE,
     contour.levels=NULL,
     contour.labels=NULL,
     station.indices,
     coastline=NULL,
     xlim=NULL,
     ylim=NULL,
     map.xlim=NULL,
     xtype="distance",
     ytype="depth",
     legend.loc="bottomright",
     adorn=NULL,
     mgp=getOption("oce.mgp"),
     mar=c(mgp[1]+1, mgp[1]+1, mgp[2], mgp[2] + 0.5),
     \dots)
}

\arguments{
  \item{x}{a \code{section} object, e.g. as created by
    \code{\link{make.section}}.}
  \item{which}{list of desired plot types.  There may be up to four panels in
    total, and the desired plots are placed in these panels, in
    reading order. Set \code{which} to 1 to contour temperature, to 2
    to contour salinity, to 3 to contour sigma.theta, or to 4 to plot
    a map of station locations.  If only one panel is plotted,
    \code{par} is not adjusted, which makes it easy to add to the plot
    with subsequent plotting commands.}
  \item{at}{if \code{NULL} (the default), the x axis will indicate the
    distance of the stations from the first in the section.  (This may
    give errors in the contouring routine, if the stations are not present
    in a geographical order.)  If a list, then it indicates the values at
    which stations will be plotted.} 
  \item{labels}{either a logical, indicating whether to put labels on
    the x axis, or a vector that is a list of labels to be placed at the x
    positions indicated by \code{at}.} 
  \item{grid}{if \code{TRUE}, points are drawn at data locations.}
  \item{contour.levels}{if not \code{NULL}, and if a \code{field} was
    specified, this specifies contour levels (see details)}
  \item{contour.labels}{if not \code{NULL}, and if a \code{field} was
    specified, this specifies contour labels (see details)}
  \item{station.indices}{optional list of the indices of stations to
    use.  Note that these are indices, not the names of stations, e.g. to
    focus on the first four stations, use \code{station.numbers=c(1,4)}.}
  \item{coastline}{optional \code{coastline} to be used in a station
    map}
  \item{xlim}{optional limit for x axis (only in sections, not map)}
  \item{ylim}{optional limit for y axis (only in sections, not map)}
  \item{map.xlim}{optional xlim for station location, which can be
    helpful in ensuring that a recognizable coastline can be seen}
  \item{xtype}{type of x axis, for contour plots, either
    \code{"distance"} for distance (in km) to the first point in the
    section, or \code{"track"} for distance along the cruise track.
    Note that if the x values are not in order, they will be put in
    order (which may make no sense) and a warning will be printed.}
  \item{ytype}{type of y axis for contour plots, either
    \code{"pressure"} for pressure (in dbar, with zero at the surface)
    or \code{"depth"} for depth (in m below the surface, calculated
    from pressure with \code{\link{sw.depth}}).}
  \item{legend.loc}{location of legend, as supplied to \code{\link{legend}}.}
  \item{adorn}{list of expressions to be executed for the panels in
    turn, e.g. to adorn the plots.  If the number matches the number of
    panels, then the strings are applied to the appropriate panels, as
    they are drawn from top-left to bottom-right.   If only a single
    expression is provided, it is used for all panels.  (See
    \dQuote{Examples}.)}
  \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
    also for \code{par(mar)}, computed from this.  The default is
    tighter than the R default, in order to use more space for the data
    and less for the axes.}
  \item{mar}{value to be used with \code{\link{par}("mar")}.}
  \item{\dots}{optional arguments passed to the contouring function,
    e.g. using \code{labcex=1} will increase the size of contour labels.}
}

\details{Creates a summary plot for a CTD section. If a \code{field} is
  supplied, then just that single field is contoured. If no \code{field}
  is supplied, then temperature, salinity, and sigma are contoured. A
  location plot is also drawn if a \code{coastline} is provided; in
  this, the first station in the section is indicated with a different
  symbol than the rest.

  The y-axis for the contours is pressure, plotted in the conventional
  reversed form, so that the water surface appears at the top of the
  plot. The x-axis is more complicated. If \code{at} is not supplied,
  then the routine calculates x as the distance between the first
  station in the section and each of the other stations. (This will
  produce an error if the stations are not ordered geographically,
  because the \code{\link{contour}} routine cannot handle non-increasing
  axis coordinates.) If \code{at} is specified, then it is taken to be
  the location, in arbitrary units, along the x-axis of labels specified
  by \code{labels}; the way this works is designed to be the same as for
  \code{\link{axis}}.
}

\value{None.}

\seealso{Sections may be created with \code{\link{make.section}} or
  \code{\link{read.section}}, and may be summarized with
  \code{\link{summary.section}}.}

\examples{
library(oce)
# Halifax Harbour
data(section)
data(coastline.hal)
plot(section, coastline=coastline.hal)

# Gulf Stream
data(a03)
Gulf.Stream <- subset(a03, indices=124:102)
Gulf.Stream.gridded <- section.grid(Gulf.Stream, p=seq(0,2000,100))
data(coastline.world)
plot(Gulf.Stream.gridded, coastline=coastline.world, map.xlim=c(-80,-60))
# Illustate adornment
plot(Gulf.Stream.gridded, coastline=coastline.world, map.xlim=c(-80,-60),
     adorn=expression({abline(v=200,col='blue')}))
}

\author{Dan Kelley}

\keyword{misc}
