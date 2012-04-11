\name{julianDay}

\alias{julianDay}

\title{Convert a POSIXt time to a Julian-Day number}

\description{Convert a POSIXt time to a Julian-Day number}

\usage{julianDay(t, year, month, day, hour, min, sec, tz="UTC")}

\arguments{

    \item{t}{a time, in POSIXt format, e.g. as created by
        \code{\link{as.POSIXct}}, \code{\link{as.POSIXlt}}, or
        \code{\link{numberAsPOSIXct}}.  If this is provided, the other
        arguments are ignored.}

    \item{year}{year, to be provided along with \code{month}, etc., if \code{t} is not provided.}

    \item{month}{month, numbered with January being 1.}

    \item{day}{day in month, starting at 1.}

    \item{hour}{hour of day.}

    \item{min}{minute of hour}

    \item{sec}{second of hour}

    \item{tz}{timezone}

}

\details{The time is represented in astronomical convention, and the
    calculation is done with the method illustrated in the documentation for
    \code{\link{julian}}.  The method is as described in Chapter 3 of Meeus
    (1982), which provides the example given here.}

\value{A Julian-Day number.}

\examples{
t <- ISOdatetime(1977, 4, 26, hour=0, min=0, sec=0, tz="ET")+0.4*86400
jd <- julianDay(t)
cat(format(t), ": Julian Day", format(jd, digits=14), "; Meeus [page 23, ch 3] gives 2443259.9\n")
}

\author{Dan Kelley}

\references{Meeus, Jean, 1982.  Astronomical formuae for Calculators.
    Willmann-Bell. Richmond VA, USA. 201 pages}

\keyword{misc}

