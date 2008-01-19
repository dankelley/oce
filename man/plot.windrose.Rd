\name{plot.windrose}
\alias{plot.windrose}
\title{Plot a wind rose diagram}
\description{Plot a wind-rose diagram}
\usage{plot.windrose(x, type=c("count","mean","median","fivenum"), col, \dots)}
}
\arguments{
  \item{x}{a \code{windrose} object, e.g. as created by \code{\link{as.windrose}}.}
  \item{type}{the thing to be plotted, either the number of counts in
	the angle interval, the mean of the values in the interval, the
	median of the values, or a \code{\link{fivenum}} representation of
	the values.}
  \item{col}{an optional list of colours to use.  If not set, the
    colours will be \code{c("red", "pink", "blue", "lightgray")}.  For
    the first three types of plot, the first colour in this list is 
    used to fill in the rose, the third is used for the petals of the
    rose, and the fourth is used for grid lines. For the
    \code{"fivenum"} type, the first colour is used for the
    inter-quartile range, the second is used outside this range, the 
    third is used for the median, and the fourth is, again, used for 
    the grid lines.}   
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Creates a wind-rose diagram.}

%\value{None.}

\seealso{\code{\link{as.windrose}} creates a wind-rose object, and
  \code{\link{summary.windrose}} produces a numerical summary.}

\examples{
library(oce)
xcomp <- rnorm(360) + 1
ycomp <- rnorm(360)
wr <- as.windrose(xcomp, ycomp)
par(mfrow=c(1,2))
plot(wr)
plot(wr, "fivenum")
}

\author{Dan Kelley}
\keyword{misc}
