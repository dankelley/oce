\name{plot.etopo2}
\alias{plot.etopo2}
\title{Plot topography (ETOPO2) data}
\description{Plot a summary diagram for ETOPO2 data.}
\usage{\method{plot}{etopo2}(x,\dots)}
\arguments{
  \item{x}{an \code{etopo2} object, e.g. as read by \code{\link{read.etopo2}}.}
  \item{\dots}{optional arguments passed to plotting functions.}
}
\details{Be aware that the ETOPO2 data are on a 2-minute (roughly
  2-mile) spacing, and coastlines created with such data can be quite
  inaccurate on scales of 100km or less.
}
\seealso{ETOPO2 data can be created with \code{\link{read.etopo2}} and
  summarized with \code{\link{summary.etopo2}}.
}
\examples{
library(oce)
data(bof)
plot(bof)
}
\author{Dan Kelley}
\keyword{misc}
