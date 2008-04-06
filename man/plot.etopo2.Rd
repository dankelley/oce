\name{plot.etopo2}

\alias{plot.etopo2}

\title{Plot topography (ETOPO2) data}

\description{Plot a summary diagram for ETOPO2 data.}

\usage{\method{plot}{etopo2}(x, water.z, water.colors, land.z,
land.colors, legend.loc="topright", \dots)}

\arguments{
  \item{x}{an \code{etopo2} object, e.g. as read by \code{\link{read.etopo2}}.}
  \item{water.z}{depths at which to plot water contours.  If not
    provided, these are inferred from the data.}
  \item{water.colors}{colors corresponding to \code{water.z} values.  If
    not provided, these will be \code{"fill"} colors from
    \code{\link{gebco.colors}}.}
  \item{land.z}{depths at which to plot land contours.  If not provided,
    these are inferred from the data.} 
  \item{land.colors}{colors corresponding to \code{land.z} values.  If
    not provided, these will be \code{"fill"} colors from
    \code{\link{gebco.colors}}.}
  \item{legend.loc}{location for a legend (or FALSE, for no legend).}
  \item{\dots}{additional arguments passed on to plotting functions}
}

\details{The plot aspect ratio is set based on the middle latitude in
  the plot.  Be aware that the ETOPO2 data are on a 2-minute (roughly
  2-mile) spacing, and coastlines created with such data can be quite
  inaccurate on scales of 100km or less.
}

\seealso{ETOPO2 data can be created with \code{\link{read.etopo2}} and
  summarized with \code{\link{summary.etopo2}}.
}

\examples{
library(oce)
data(topo.maritimes)
plot(topo.maritimes)
}

\author{Dan Kelley}

\keyword{misc}
