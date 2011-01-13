\name{as.ctd}
\alias{as.ctd}
\title{Coerce data into ctd dataset}
\description{Coerces a dataset into a ctd dataset.}

\usage{
as.ctd(salinity, temperature, pressure, ship=NA, scientist=NA,
   institute=NA, address=NA, cruise=NA, station=NA, date=NA,
   start.time=NA, latitude=NA, longitude=NA, recovery=NA,
   water.depth=NA, sample.interval=NA, src="")
}
\arguments{
  \item{salinity}{Salinity through the water column, or a data frame
    containing columns named \code{salinity}, \code{temperature}, and
    \code{pressure}, in which case these values are extracted from the
    data frame, and the next two arguments are ignored.  Otherwise, 
    if it is numeric, it is first converted to a vector before proceeding.} 
  \item{temperature}{Temperature through the water column. (This is converted to a vector,
     if it is not one already.}
  \item{pressure}{pressure through the water column. (If just a single value 
     is given, then it is repeated to match the length of the temperature and salinity.}
  \item{ship}{optional string containing the ship from which the observations were made.}
  \item{scientist}{optional string containing the chief scientist on the cruise.}
  \item{institute}{optional string containing the institute behind the work.}
  \item{address}{optional string containing the address of the institute.}
  \item{cruise}{optional string containing a cruise identifier.}
  \item{station}{optional string containing a station identifier.}
  \item{date}{optional string containing the date at which the profile was started.}
  \item{start.time}{optional string containing the start time.}
  \item{latitude}{optional numerical value containing the latitude in decimal degrees, positive in the northern hemisphere.}
  \item{longitude}{optional numerical value containing longitude in decimal degrees, positive in the eastern hemisphere.}
  \item{recovery}{optional string indicating the recovery time.}
  \item{water.depth}{optional numerical value indicating the water depth in metres.}
  \item{sample.interval}{optional numerical value indicating the time
    between samples in the profile.}
  \item{src}{optional string indicating data source}
}

\details{This function assembles vectors of salinity, temperature, and
  pressure, to create a \code{ctd} object, e.g. so that
  \code{\link{plot.ctd}} can be used to make a standard four-panel plot,
  or so that a section can be constructed with
  \code{\link{make.section}}.  Normally, the input vectors will be of
  the same length, but \code{as.ctd} can also handle cases in which one
  or two of these is of unit length.  For example, if only a temperature
  profile is available, \code{as.ctd(35, T, p)} could be used to
  construct a \code{ctd} object with constant salinity.}

\value{An object of \code{\link[base]{class}} \code{"ctd"} (for details,
  see \code{\link{read.ctd}}).}

\seealso{A file containing CTD profile data may be read with
  \code{\link{read.ctd}}.  A \code{ctd} object may be summarized with
  \code{\link{summary.ctd}}.  Overview plots may be made with
  \code{\link{plot.ctd}}, while \code{\link{plot.TS}} produces TS plots
  and \code{\link{plot.ctd.scan}} produces scan plots that may help with
  data editing.  Extraneous data such as those collected during upcasts
  and equilibration intervals may be trimmed with
  \code{\link{ctd.trim}}, and the data may be cast onto specified
  pressure levels with \code{\link{ctd.decimate}}.  Low-level
  manipulation may be done with \code{\link{ctd.add.column}} and
  \code{\link{ctd.update.header}}, as well as by direct manipulation of
  the items within \code{ctd} objects.}

\examples{
library(oce)
pressure <- 1:50
temperature <- 10 - tanh((pressure - 20) / 5) + 0.02*rnorm(50)
salinity <- 34 + 0.5*tanh((pressure - 20) / 5) + 0.01*rnorm(50)
ctd <- as.ctd(salinity, temperature, pressure)
summary(ctd)
plot(ctd)
}

\author{Dan Kelley}

\keyword{misc}
