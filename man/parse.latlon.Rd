\name{parse.latlon}

\alias{parse.latlon}

\title{Parse a latitude or longitude string}

\description{Parse a latitude or longitude string, e.g. as in the header of a CTD file}

\usage{parse.latlon(line, north=TRUE, debug=FALSE)}

\arguments{
  \item{line}{a character string containing an indication of latitude or longitude.}
  \item{debug}{set to TRUE to make this function print its processing steps.}
}

\value{A numerical value of latitude or longitude.}

\details{
  The following formats are understood (for, e.g. latitude)
\preformatted{
* NMEA Latitude = 47 54.760 N
** Latitude:      47 53.27 N
}
}

\seealso{Used by \code{\link{read.ctd}}.}

\author{Dan Kelley}

\keyword{misc}
