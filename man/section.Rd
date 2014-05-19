\name{section}

\docType{data}

\alias{section}

\title{Hydrographic section}

\description{This is line A03 (ExpoCode 90CT40_1, with nominal sampling date
    1993-09-11).  The chief scientist was Tereschenkov of SOI, working aboard
    the Russian ship Multanovsky, undertaking a westward transect from the
    Mediterranean outflow region across to North America, with a change of
    heading in the last few dozen stations to run across the nominal Gulf
    Stream axis.}

\examples{
\dontrun{
library(oce)
# Gulf Stream
data(section)
GS <- subset(section, 109<=stationId&stationId<=129)
GSg <- sectionGrid(GS, p=seq(0, 5000, 100))
plot(GSg, map.xlim=c(-80,-60))
}
}

\usage{data(section)}

\source{This is based on a WOCE file downloaded from
    \url{http://cchdo.ucsd.edu/data/onetime/atlantic/a03/a03_hy1.csv}.}

\seealso{The documentation for \code{section-class} in the Oce package explains
    the structure of section objects, and also outlines the functions dealing
    with them.}

\author{Dan Kelley}

\keyword{datasets}
