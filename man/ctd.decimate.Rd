\name{ctd.decimate}
\alias{ctd.decimate}
\title{Decimate a CTD profile}
\description{
Smooth and decimate a CTD profile.  
}
\usage{
ctd.decimate(x, p=NULL, method=c("boxcar","lm"), e=1)
}
\arguments{
\item{x}{a \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.}
\item{p}{controls the selection of pressure levels, as follows.
If \code{NULL} (the default), then the list of pressures for the decimation is to be selected automatically, using \code{\link{pretty}}.  If a single
value is supplied, then the pressure list starts at the surface and increments
with the indicated value, down to the bottom.  If a list is supplied,
then this is taken to be the desired pressure list.}
\item{method}{Method for calculating decimated values.
Use \code{"boxcar"} (the default)
for boxcar averaging within the pressure region.
Use \code{"lm"} to use the prediction of a linear
model applied to each pressure region.    (See \code{e}
for an explanation of the region.)}
\item{e}{is an expansion coefficient used to calculate the smoothing neighbourhoods.
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
xd$data$sigma <- sw.sigma(xd$data$salinity, xd$data$temperature, xd$data$pressure)
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
