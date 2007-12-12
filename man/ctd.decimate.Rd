\name{ctd.decimate}
\alias{ctd.decimate}
\title{Decimate a CTD profile}
\description{
Smooth and decimate a CTD profile.  
}
\usage{
ctd.decimate(x, p, method=c("boxcar","lm"), e=1)
}
\arguments{
\item{x}{a \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.}
\item{p}{an optional pressure increment, or series of pressures.  (a) If this is not supplied, then the list of pressures for the decimation is to determined automatically, using \code{\link{pretty}} on the mean inter-sample pressure difference.  (b) If a single
value is supplied, then the pressure list starts at the surface and increments
with the indicated value, down to the bottom.  (c) If a list is supplied, then values are interpolated to these pressures.}
\item{method}{Method for calculating decimated values.
Use \code{"approx"} (the default) for linear interpolation between
neighboring points.  If some element of smoothing is desired,
use \code{"boxcar"}
for boxcar averaging within the pressure region, or
\code{"lm"} for prediction based on a linear
model applied to each pressure region (see
\code{e} for an explanation of the region).)}
\item{e}{is an expansion coefficient used to calculate the smoothing neighbourhoods for the \code{"boxcar"} and \code{"lm"} methods.
If \code{e=1}, then the neighbourhood for the i-th pressure extends from the (\code{i-1})-th pressure
to the (\code{i+1})-th pressure.  (At the endpoints it is assumed that the outside bin is of the
same pressure range as the first inside bin.)  For other values of \code{e}, the neighbourhood
is expanded linearly in each direction.}
}
\details{
This function is useful for calculations that require data to be defined at a fixed set
of pressure values. Normally, the target pressures are supplied by the user (e.g. to
simplify thermal-wind calculations based on pairs of CTD profiles), but the routine can
select the target pressures automatically. The smoothing part of the algorithm is based
on neighbourhoods of each target pressures.

Note that the density that results from the decimation is calculated from an average, and
so it may not match with the averaged salinity and temperature. This is a sort of
numerical cabeling effect, and if you would like to avoid this, just do as follows

\preformatted{
xd <- ctd.decimate(x)
xd$data$sigma.theta <- sw.sigma.theta(xd$data$salinity, xd$data$temperature, xd$data$pressure)
}

}
\value{
A new \code{ctd} object.
}

\seealso{
A \code{ctd} object may be read with \code{\link{read.ctd}},
and \code{\link{ctd.trim}} is useful in trimming spurious data
(e.g. those obtained during the upcast).
}
\examples{
library(oce)
data(ctd.raw) 
ctd.clean <- ctd.decimate(ctd.trim(ctd.raw))
summary(ctd.clean)
}
\references{
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}
