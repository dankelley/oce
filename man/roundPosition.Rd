\name{roundPosition}

\alias{roundPosition}

\title{Round a geographical positions in degrees, minutes, and seconds}

\description{Round a geographical positions in degrees, minutes, and seconds}

\usage{roundPosition(x)}

\arguments{
  \item{x}{a series of one or more values of a latitude or longitude, in
      decimal degrees}
}

\details{Depending on the range of values in \code{x}, rounding is done to
    degrees, half-degrees, minutes, etc.}

\value{A vector of numbers that will yield good axis labels if
    \code{\link{formatPosition}} is used.}

\examples{
library(oce)
formatPosition(roundPosition(10+1:10/60+2.8/3600))
}

\author{Dan Kelley}

\keyword{misc}
