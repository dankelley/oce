\name{siderealTime}

\alias{siderealTime}

\title{Convert a POSIXt time to a sidereal time}

\description{Convert a POSIXt time to a sidereal time}

\usage{siderealTime(t)}

\arguments{
    \item{t}{a time, in POSIXt format, e.g. as created by
        \code{\link{as.POSIXct}}, \code{\link{as.POSIXlt}}, or
        \code{\link{numberAsPOSIXct}}.  If this is provided, the other
        arguments are ignored.}
}

\details{The method is taken from Chapter 7 of Meeus (1982).}

\value{A sidereal time.}

\examples{
t <- ISOdatetime(1978, 11, 13, 0, 0, 0, tz="UTC")
print(siderealTime(t))
}

\author{Dan Kelley}

\references{Meeus, Jean, 1982.  Astronomical formuae for Calculators.
    Willmann-Bell. Richmond VA, USA. 201 pages}

\keyword{misc}

