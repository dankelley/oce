\name{plot.topo}

\alias{plot.topo}

\title{Plot topography data}

\description{Plot contours of topographic data.}

\usage{\method{plot}{topo}(x, 
  water.z, water.col, water.lty, water.lwd,
  land.z,   land.col,  land.lty,  land.lwd,
  legend.loc="topright", \dots)}

\arguments{
  \item{x}{an \code{topo} object, e.g. as read by \code{\link{read.topo}}.}
  \item{water.z}{depths at which to plot water contours.  If not
    provided, these are inferred from the data.}
  \item{water.col}{colors corresponding to \code{water.z} values.  If
    not provided, these will be \code{"fill"} colors from
    \code{\link{gebco.colors}}.}
  \item{water.lty}{line type(s) for water contours}
  \item{water.lwd}{line width(s) for water contours}
  \item{land.z}{depths at which to plot land contours.  If not provided,
    these are inferred from the data.} 
  \item{land.col}{colors corresponding to \code{land.z} values.  If
    not provided, these will be \code{"fill"} colors from
    \code{\link{gebco.colors}}.}
  \item{land.lty}{line type(s) for land contours}
  \item{land.lwd}{line width(s) for land contours}
  \item{legend.loc}{location for a legend (or FALSE, for no legend).}
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
