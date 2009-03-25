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
\dontrun{
library(oce)
d <- read.oce(file = "~/SL08F001.000", skip = 5000, read = 6 * 30)
par(mfrow=c(2,1))
plot(d, which=5) # beam 1 echo intensity
da <- adcp.beam.attenuate(d)
plot(da, which=5) # beam 1 echo intensity
}
}

\references{The coefficient to convert to decibels is a personal
  communication.  The logarithmic term is explained in textbooks on
  acoustics, optics, etc.}

\author{Dan Kelley}

\keyword{misc}
