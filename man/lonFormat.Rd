\name{lonFormat}

\alias{lonFormat}

\title{Format a longitude}

\description{Format a longitude, using "W" for west longitude.}

\usage{lonFormat(lon, digits=max(6, getOption("digits") - 1))}

\arguments{
  \item{lon}{longitude in \eqn{^\circ}{deg}N east of Greenwich.}
  \item{digits}{the number of significant digits to use when printing.}
}

\value{A character string.}

\seealso{\code{\link{latFormat}} and \code{\link{latlonFormat}}.}

\author{Dan Kelley}

\keyword{misc}
