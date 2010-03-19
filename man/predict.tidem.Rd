\name{predict.tidem}
\alias{predict.tidem}
\title{Predict a time series from a tidal model}
\description{Predict a time series from a tidal model.}
\usage{\method{predict}{tidem}(object, ...)}
\arguments{
  \item{object}{a \code{tidem} object.}
  \item{\dots}{optional arguments passed on to children.}
}

\details{This is a wrapper around the predict method for
  \code{object$model}.}

\value{A vector of predictions.}

\seealso{\code{\link{tidem}} fits a tidal model.}

\examples{
library(oce)
data(sealevel.tuk)
tide <- tidem(sealevel.tuk)
plot(sealevel.tuk$data$time, sealevel.tuk$data$elevation, type='l', ylim=c(-2,5))
lines(sealevel.tuk$data$time, sealevel.tuk$data$elevation - predict(tide), col="red")
abline(h=0, col="red")
}

\author{Dan Kelley}
\keyword{misc}
