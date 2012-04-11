\name{julianCenturyAnomaly}

\alias{julianCenturyAnomaly}

\title{Convert a Julian-Day number to a time in julian centuries since noon on
    January 1, 1900.}

\description{Convert a Julian-Day number to a time in julian centuries since
    noon on January 1, 1900.}

\usage{julianCenturyAnomaly(jd)}

\arguments{
    \item{jd}{a julian day number, e.g. as given by \code{\link{julianDay}}.}
}

\details{The method uses equation 15.1 in chapter 15 of Meeus (1982).}

\value{A Julian-Day number, in astronomical convention as explained in Meeus.}

\examples{
t <- ISOdatetime(1977, 4, 26, hour=0, min=0, sec=0, tz="ET")+0.4*86400
jd <- julianDay(t)
jc <- julianCenturyAnomaly(jd)
cat(format(t),
    "is Julian Day", format(jd, digits=14),
    " Julian Century", format(jc, digits=3), "\n")
}

\author{Dan Kelley}

\references{Meeus, Jean, 1982.  Astronomical formuae for Calculators.
    Willmann-Bell. Richmond VA, USA. 201 pages}

\keyword{misc}

