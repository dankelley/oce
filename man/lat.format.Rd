\name{lat.format}

\alias{lat.format}

\title{Format a latitude}

\description{Format a latitude, using "S" for negative latitude.}

\usage{lat.format(lat, digits=max(6, getOption("digits") - 1))}

\arguments{
  \item{lat}{latitude in \eqn{^\circ}{deg}N north of the equator.}
  \item{digits}{the number of significant digits to use when printing.}
}

\value{A character string.}

\seealso{\code{\link{lon.format}} and \code{\link{latlon.format}}.}

\author{Dan Kelley}

\keyword{misc}
