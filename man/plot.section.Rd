\name{plot.section}
\alias{plot.section}
\title{Plot a CTD section}
\description{
  Plot a CTD section.
}
\usage{
\method{plot}{section}(x,
 	field=NULL,
	at=NULL,
	labels=TRUE,
	grid=TRUE,
	col.grid="lightgray",
	coastline=NULL,
	\dots)
}
\arguments{
  \item{x}{a \code{section} object, e.g. as created by \code{\link{make.section}}.}
  \item{field}{is the field to plot.  Common options are \code{"temperature"}, \code{"salinity"}, and \code{"density"}, although \code{plot.section} will accept any named field that is present in the constituent CTD profiles. If \code{NULL} (the default) is given, then temperature, salinity and density are graphed.}
  \item{at}{if \code{NULL} (the default), the x axis will indicate the distance of the stations from the first in the section.  (This may give errors in the contouring routine, if the stations are not present in a geographical order.)  If a list, then it indicates the values at which stations will be plotted.}
  \item{labels}{either a logical, indicating whether to put labels on the x axis, or a vector that is a list of labels to be placed at the x positions indicated by \code{at}.}
  \item{grid}{if \code{TRUE}, grid lines will be drawn on plots.}
  \item{col.grid}{color of grid lines.}
  \item{coastline}{if supplied, this is a \code{coastline} object, to be used in a station map}
  \item{\dots}{optional arguments passed to plotting functions, e.g. using
     \code{labcex=1} will increase the size of contour labels.}
}
\details{
Creates a summary plot for a CTD section. If a \code{field} is supplied, then just that single
field is contoured. If no \code{field} is supplied, then temperature, salinity, and sigma are
contoured. A location plot is also drawn if a \code{coastline} is provided; in this, the first
station in the section is indicated with a different symbol than the rest.

The y-axis for the contours is pressure, plotted in the conventional reversed form, so that the
water surface appears at the top of the plot. The x-axis is more complicated. If \code{at} is
not supplied, then the routine calculates x as the distance between the first station in the
section and each of the other stations. (This will produce an error if the stations are not
ordered geographically, because the \code{\link{contour}} routine cannot handle non-increasing
axis coordinates.) If \code{at} is specified, then it is taken to be the location, in arbitrary
units, along the x-axis of labels specified by \code{labels}; the way this works is designed to
be the same as for \code{\link{axis}}.
}
\value{
  None.
}
\seealso{
Sections are created with \code{\link{make.section}}, and may
be summarized with \code{\link{summary.section}}.
}
\examples{
library(oce)
data(section)
data(coastline.halifax)
plot(section, coastline=coastline.halifax)
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}
