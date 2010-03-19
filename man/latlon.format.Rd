\name{latlon.format}

\alias{latlon.format}

\title{Format a latitude-longitude pair}

\description{Format a latitude-longitude pair, using "S" for negative latitudes, etc.}

\usage{latlon.format(lat, lon, digits=max(6, getOption("digits") - 1))}

\arguments{
  \item{lat}{latitude in \eqn{^\circ}{deg}N north of the equator.}
  \item{lon}{longitude in \eqn{^\circ}{deg}N east of Greenwich.}
  \item{digits}{the number of significant digits to use when printing.}
}

\value{A character string.}

\seealso{\code{\link{lat.format}} and \code{\link{lon.format}}.}

\author{Dan Kelley}

\keyword{misc}
