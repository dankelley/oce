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
time <- sealevelTuktoyaktuk[["time"]]
elevation <- sealevelTuktoyaktuk[["elevation"]]
oce.plot.ts(time, elevation, type='l', ylab="Height [m]", ylim=c(-2,6))
tide <- tidem(sealevelTuktoyaktuk)
lines(time, elevation - predict(tide), col="red")
abline(h=0, col="red")
#
# 2. prediction at specified times
data(sealevelHalifax)
m <- tidem(sealevelHalifax)
look <- 1:24                          # examine first day
time <- sealevelHalifax[["time"]]
elevation <- sealevelHalifax[["elevation"]]
oce.plot.ts(time[look], elevation[look])
# Notice that t is at a 10-minute interval
t <- seq(from=time[1], to=time[max(look)], by=360)
lines(t, predict(m,newdata=t), col='red')
legend("topright", col=c("black","red"),legend=c("data","model"),lwd=1)
}

\author{Dan Kelley}
\keyword{misc}
