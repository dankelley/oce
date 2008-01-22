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
data(sealevel.tuk)
tide <- tidem(sealevel.tuk)
plot(sealevel.tuk$data$t, sealevel.tuk$data$eta, type='l', ylim=c(-2,5))
lines(sealevel.tuk$data$t, sealevel.tuk$data$eta - predict(tide), col="red")
}

\author{Dan Kelley}
\keyword{misc}
