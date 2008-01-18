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
  \item{data}{a \code{\link[base]{list}} containing
	\item{n}{the number of x values}
	\item{x.mean}{the mean of the x values}
	\item{y.mean}{the mean of the y values}
	\item{theta}{the central angle (in degrees) for the class}
	\item{count}{the number of observations in this class}
	\item{mean}{the mean of the observations in this class}
	\item{fivenum}{the \code{\link{fivenum}} vector for observations in
	  this class (the min, the lower hinge, the median, the upper hinge, and
	  the max)}
  }
  \item{metadata}{a list containing
	\item{dtheta}{the size of the angle category, in degrees}
  }
  \item{processing.log}{a processing log, in the standard \code{oce} format.}
}

\seealso{Use \code{\link{plot.windrose}} to produce a summary plot, and
  \code{\link{summary.windrose}} to produce a numerical summary.}

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
