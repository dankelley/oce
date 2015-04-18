\name{as.ctd}
\alias{as.ctd}
\title{Coerce data into ctd dataset}
\description{Coerces a dataset into a ctd dataset.}

\usage{as.ctd(salinity, temperature, pressure, conductivity,
       SA, CT,
       oxygen, nitrate, nitrite, phosphate, silicate,
       scan, other,
       missingValue,
       quality, 
       filename="", type="", model="", serialNumber="",
       ship="", scientist="", institute="", address="", cruise="", station="", 
       date="", startTime="", recovery="",
       longitude=NA, latitude=NA,
       pressureAtmospheric=NA, waterDepth=NA,
       sampleInterval=NA, src="")
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

  \item{conductivity}{electrical conductivity ratio through the water column
      (optional). Some devices report raw conductivity in milliSeimens per
      centimeter, and that number should be divided by 42.914 to get
      \code{conductivity} for use here.}

  \item{SA}{absolute salinity (as in TEOS-10).  If given, the supplied absolute
      salinity is converted internally to UNESCO-defined practical salinity.}
  \item{CT}{conservative temperature (as in TEOS-10).  If given, the supplied
      conservative temperature is converted internally to UNESCO-defined
      in-situ temperature.}
  \item{oxygen}{optional oxygen concentration}
  \item{nitrate}{optional nitrate concentration [micromole/kg]}
  \item{nitrite}{optional nitrite concentration [micromole/kg]}
  \item{phosphate}{optional phosphate concentration [micromole/kg]}
  \item{silicate}{optional silicate concentration [micromole/kg]}
  \item{scan}{optional scan number.  If not provided, this will be set
      to \code{1:length(salinity)}.}
  \item{other}{optional list of other data columns that are not in the standard list}
  \item{missingValue}{optional missing value, indicating data that should be
      taken as \code{NA}.}
  \item{quality}{quality flag, e.g. from the salinity quality flag in WOCE
     data.  (In WOCE, \code{quality=2} indicates good data, 
     \code{quality=3} means questionable data, and \code{quality=4}
     means bad data.}
  \item{filename}{filename to be stored in the object}
  \item{type}{type of CTD, e.g. "SBE"}
  \item{model}{model of instrument}
  \item{serialNumber}{serial number of instrument}
  \item{ship}{optional string containing the ship from which the observations were made.}
  \item{scientist}{optional string containing the chief scientist on the cruise.}
  \item{institute}{optional string containing the institute behind the work.}
  \item{address}{optional string containing the address of the institute.}
  \item{cruise}{optional string containing a cruise identifier.}
  \item{station}{optional string containing a station identifier.}
  \item{date}{optional string containing the date at which the profile was started.}
  \item{startTime}{optional string containing the start time.}
  \item{recovery}{optional string indicating the recovery time.}
  \item{longitude}{optional numerical value containing longitude in decimal degrees, positive in the eastern hemisphere.}
  \item{latitude}{optional numerical value containing the latitude in decimal degrees, positive in the northern hemisphere.}

  \item{pressureAtmospheric}{if provided, this value is subtracted from
      pressure data before storing them in the object.}

  \item{waterDepth}{optional numerical value indicating the water depth in metres.}

  \item{sampleInterval}{optional numerical value indicating the time
    between samples in the profile.}
  \item{src}{optional string indicating data source}
}

\details{This function assembles vectors of salinity, temperature, and
  pressure, to create a \code{ctd} object, e.g. so that
  \code{\link{plot.ctd}} can be used to make a standard four-panel plot,
  or so that a section can be constructed with
  \code{\link{makeSection}}.  Normally, the input vectors will be of
  the same length, but \code{as.ctd} can also handle cases in which one
  or two of these is of unit length.  For example, if only a temperature
  profile is available, \code{as.ctd(35, T, p)} could be used to
  construct a \code{ctd} object with constant salinity.}

\value{An object of \code{\link[base]{class}} \code{"ctd"} (for details,
  see \code{\link{read.ctd}}).}

\seealso{The documentation for \code{\link{ctd-class}} explains the structure
    of CTD objects, and also outlines the other functions dealing with them.}


\examples{
library(oce)
pressure <- 1:50
temperature <- 10 - tanh((pressure - 20) / 5) + 0.02*rnorm(50)
salinity <- 34 + 0.5*tanh((pressure - 20) / 5) + 0.01*rnorm(50)
ctd <- as.ctd(salinity, temperature, pressure)
summary(ctd)
plot(ctd)
}

\references{
\url{http://cchdo.ucsd.edu/static/information/policies/CCHDO_DataSubmitGuide.pdf}
\url{http://cchdo.ucsd.edu/static/information/manuals/pdf/90_1/chap4.pdf}
}

\author{Dan Kelley}

\keyword{misc}
