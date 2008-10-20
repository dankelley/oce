\name{plot.section}

\alias{plot.section}

\title{Plot a CTD section}

\description{Plot a CTD section.}

\usage{
\method{plot}{section}(x,
 	field=NULL,
	at=NULL,
	labels=TRUE,
	grid=FALSE,
        contour.levels=NULL,
        contour.labels=NULL,
	station.indices,
	coastline=NULL,
	map.xlim=NULL,
	\dots)
}

\arguments{
  \item{x}{a \code{section} object, e.g. as created by
    \code{\link{make.section}}.}
  \item{field}{is the field to plot.  Common options are
    \code{"temperature"}, \code{"salinity"}, and \code{"density"},
    although \code{plot.section} will accept any named field that is
    present in the constituent CTD profiles. If \code{NULL} (the default)
    is given, then temperature, salinity and density are graphed.  If a
    field is specified, then it is also possible to specify the
    \code{contour.levels} and \code{contour.labels} (see below).}
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
  \item{map.xlim}{optional xlim for station location, which can be
    helpful in ensuring that a recognizable coastline can be seen}
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
Gulf.Stream <- section.subset(a03, 124:102)
Gulf.Stream.gridded <- section.grid(Gulf.Stream, p=seq(0,2000,10))
data(coastline.world)
plot(Gulf.Stream.gridded, coastline=coastline.world, map.xlim=c(-80,-60))
}

\author{Dan Kelley}

\keyword{misc}
