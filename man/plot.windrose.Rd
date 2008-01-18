\name{plot.windrose}
\alias{plot.windrose}
\title{Plot a wind rose diagram}
\description{Plot a wind-rose diagram}
\usage{plot.windrose(x, type=c("count","mean","median","fivenum"), \dots)}
}
\arguments{
  \item{x}{A \code{windrose} object, e.g. as created by \code{\link{as.windrose}}.}
  \item{type}{the thing to be plotted, either the number of counts in
	the angle interval, the mean of the values in the interval, the
	median of the values, or a \code{\link{fivenum}} representation of
	the values.}
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Creates a wind-rose diagram.}

%\value{None.}

\seealso{\code{\link{as.windrose}} creates a wind-rose object.}

\examples{
library(oce)
xcomp <- rnorm(360) + 1
ycomp <- rnorm(360)
wr <- as.windrose(xcomp, ycomp)
plot(wr)
}

\author{Dan Kelley}
\keyword{misc}
