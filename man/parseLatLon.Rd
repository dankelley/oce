\name{parseLatLon}

\alias{parseLatLon}

\title{Parse a latitude or longitude string}

\description{Parse a latitude or longitude string, e.g. as in the header of a CTD file}

\usage{parseLatLon(line, debug=getOption("oceDebug"))}

\arguments{
  \item{line}{a character string containing an indication of latitude or longitude.}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
    moderate amount of debugging information, or to 2 to get more.} 
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
