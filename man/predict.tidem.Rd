\name{predict.tidem}
\alias{predict.tidem}
\title{Predict a time series from a tidal model}
\description{Predict a time series from a tidal model.}
\usage{\method{predict}{tidem}(object, newdata, ...)}
\arguments{
  \item{object}{a \code{tidem} object.}
  \item{newdata}{optional vector of POSIXt times at which to make the
    prediction.  If not present, \code{predict.tidem} uses the times
    that were provided in the original call to \code{\link{tidem}}.}
  \item{\dots}{optional arguments passed on to children.}
}

\details{This is a wrapper around the predict method for
  \code{object$model}.}

\value{A vector of predictions.}

\seealso{\code{\link{tidem}} fits a tidal model.}

\examples{
library(oce)
#
# 1. tidal anomaly
data(sealevelTuktoyaktuk)
tide <- tidem(sealevelTuktoyaktuk)
oce.plot.ts(sealevelTuktoyaktuk@data$time, sealevelTuktoyaktuk@data$elevation,
  type='l', ylim=c(-2,5))
lines(sealevelTuktoyaktuk@data$time, sealevelTuktoyaktuk@data$elevation - predict(tide),
  col="red")
abline(h=0, col="red")
#
# 2. prediction at specified times
data(sealevelHalifax)
m <- tidem(sealevelHalifax)
look <- 1:24                          # examine first day
oce.plot.ts(sealevelHalifax@data$time[look], sealevelHalifax@data$elevation[look])
# Notice that t is at a 10-minute interval
t <- seq(from=sealevelHalifax@data$time[1], 
  to=sealevelHalifax@data$time[max(look)], by=360)
lines(t, predict(m,newdata=t), col='red')
legend("topright", col=c("black","red"),legend=c("data","model"),lwd=1)
}

\author{Dan Kelley}
\keyword{misc}
