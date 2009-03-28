\name{adcp.beam.attenuate}

\alias{adcp.beam.attenuate}

\title{Convert ADCP from beam to velocity}

\description{Convert ADCP from beam to velocity}

\usage{adcp.beam.attenuate(x, count2db=c(0.45, 0.45, 0.45, 0.45))}

\arguments{
  \item{x}{an object of class \code{"adcp"}}
  \item{count2db}{a set of coefficients, one per beam, to convert from
  beam echo intensity to decibels.} 
}

\details{Beam echo intensity is first converted from counts to decibels,
  by multiplying by \code{count2db}.  Then the signals are range
  corrected by adding the term \eqn{20\log10(r)}{20*log10(r)}, where
  \eqn{r}{r} is the distance from the sensor head to the water from
  which scattering is occuring.  \eqn{r}{r} is given by
  \code{x$data$distance}.
}

\value{An object of \code{\link[base]{class}} \code{"adcp"}.}

\examples{
library(oce)
data(adcp)
par(mfrow=c(3,1))
plot(adcp, which=5) # beam 1 echo intensity

adcp.att <- adcp.beam.attenuate(adcp)
plot(adcp.att, which=5) # beam 1 echo intensity

par(mar=c(4, 4, 1, 1))
plot(apply(adcp$data$ei1, 2, mean), adcp$data$distance, type='l', xlim=c(0,256))
lines(apply(adcp.att$data$ei1, 2, mean), adcp$data$distance, type='l', col='red')
legend("topright", lwd=1, col=c("black", "red"), legend=c("original", "attenuated"))
}

\references{The coefficient to convert to decibels is a personal
  communication.  The logarithmic term is explained in textbooks on
  acoustics, optics, etc.}

\author{Dan Kelley}

\keyword{misc}
