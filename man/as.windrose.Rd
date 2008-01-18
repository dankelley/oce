\name{as.windrose}
\alias{as.windrose}
\title{Create a windrose object}
\description{Create a wind-rose object, typically for plotting with plot.windrose().}
\usage{as.windrose(x, y, dtheta = 15)}
\arguments{
  \item{x}{the x component of wind speed (or stress).}
  \item{y}{the y component of wind speed (or stress).}
  \item{dtheta}{the angle increment (in degrees) within which to classify the data}
}
\details{
This is analagous to a histogram, but with breaks being angles.
}

\value{An object of \code{\link[base]{class}} \code{"windrose"},
  containing
  \item{theta}{the central angle (in degrees) for the class}
  \item{count}{the number of observations in this class}
  \item{mean}{the mean of the observations in this class}
  \item{fivenum}{the \code{\link{fivenum}} vector for observations in
  this class (the min, the lower hinge, the median, the upper hinge, and
  the max)}
}

\seealso{Use \code{\link{plot.windrose}} to produce a summary plot.}
\examples{
library(oce)
xcomp <- rnorm(360) + 1
ycomp <- rnorm(360)
wr <- as.windrose(xcomp, ycomp)
summary(wr)
plot(wr)
}
\references{
}
\author{Dan Kelley}
\keyword{misc}
