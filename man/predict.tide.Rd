\name{predict.tide}
\alias{predict.tide}
\title{Predict a time series from a tidal model}
\description{Predict a time series from a tidal model.}
\usage{predict.tide(object, ...)}
\arguments{
  \item{object}{a \code{tide} object.}
  \item{\dots}{optional arguments passed on to children.}
}

\details{This is a wrapper around the predict method for
  \code{object$model}.}

\value{A vector of predictions.}

\seealso{\code{\link{tidem}} fits a tidal model.}

\examples{
library(oce)
# Fake a M2 tide with phase 45 degrees
hour <- seq(0, 24 * 100)
eta <- 1 + 2 * sin(0.08051140 * 2 * pi * hour + pi/4)
sealevel <- as.sealevel(eta)
tide <- tidem(sealevel)
summary(tide)
plot(tide)
lines(hour, predict(tide))
}

\author{Dan Kelley}
\keyword{misc}
