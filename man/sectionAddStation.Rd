\name{sectionAddStation}

\alias{sectionAddCtd}
\alias{sectionAddStation}

\title{Add CTD profile to section}

\description{Add a CTD profile to an existing section.}

\usage{sectionAddStation(section, station)}

\arguments{
  \item{section}{a section to which a station is to be added.}
  \item{station}{a ctd object holding data for the station to be added.}
}

\details{Until March 2015, this operation was carried out with the \code{+}
    operator, but at that time, the syntax was flagged by the development
    version of R, so it was changed to the present form.}

\value{An object of \code{\link[base]{class}} \code{section}.}

\examples{
library(oce)
data(ctd)
ctd2 <- ctd
ctd2[["temperature"]] <- ctd2[["temperature"]] + 0.5
ctd2[["latitude"]] <- ctd2[["latitude"]] + 0.1
section <- as.section(c("ctd", "ctd2"))
ctd3 <- ctd
ctd3[["temperature"]] <- ctd[["temperature"]] + 1
ctd3[["latitude"]] <- ctd[["latitude"]] + 0.1
ctd3[["station"]] <- "Stn 3"
sectionAddStation(section, ctd3)
}

\author{Dan Kelley}

\keyword{misc}
