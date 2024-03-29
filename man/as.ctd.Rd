% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ctd.R
\name{as.ctd}
\alias{as.ctd}
\title{Coerce Data Into a ctd Object}
\usage{
as.ctd(
  salinity,
  temperature = NULL,
  pressure = NULL,
  conductivity = NULL,
  scan = NULL,
  time = NULL,
  units = NULL,
  flags = NULL,
  missingValue = NULL,
  type = "",
  serialNumber = NULL,
  ship = NULL,
  cruise = NULL,
  station = NULL,
  startTime = NULL,
  longitude = NULL,
  latitude = NULL,
  deploymentType = "unknown",
  pressureAtmospheric = 0,
  sampleInterval = NULL,
  profile = NULL,
  debug = getOption("oceDebug")
)
}
\arguments{
\item{salinity}{may be (1) a numeric vector holding Practical Salinity,
(2) a list or data frame holding \code{salinity} and other
hydrographic variables or (3) an \code{oce-class} object that holds
hydrographic information. If \code{salinity} is not provided,
then \code{conductivity} must be provided, so that \code{\link[=swSCTp]{swSCTp()}}
can be used to compute salinity.}

\item{temperature}{a numeric vector containing \emph{in-situ} temperature in
\eqn{^\circ}{deg}C on the ITS-90 scale; see \dQuote{Temperature units} in the
documentation for \code{\link[=swRho]{swRho()}}.}

\item{pressure}{a numeric vector containing sea pressure values, in decibars.
Typically, this vector has the same length as \code{salinity} and \code{temperature},
but it also possible to supply just one value, which will be repeated
to get the right length. Note that \code{as.ctd()} stores the
sum of \code{pressure} and \code{pressureAtmospheric} in the returned object,
although the default value for \code{pressureAtmospheric} is zero, so
in the default case, \code{pressure} is stored directly.}

\item{conductivity}{an optional numeric vector containing electrical
conductivity ratio through the water column. To convert from raw conductivity
in milliSeimens per centimeter divide by 42.914 to get conductivity ratio
(see Culkin and Smith, 1980).}

\item{scan}{optional numeric vector holding scan number.  If not provided,
this is set to \link{seq_along}\code{(salinity)}.}

\item{time}{optional vector of times of observation.}

\item{units}{an optional list containing units.  If not supplied,
defaults are set for \code{pressure}, \code{temperature}, \code{salinity},
and \code{conductivity}. Since these are simply guesses, users
are advised strongly to supply \code{units}. See \dQuote{Examples}.}

\item{flags}{if supplied, this is a \link{list} containing data-quality
flags. The elements of this list must have names that match the data
provided to the object.}

\item{missingValue}{optional missing value, indicating data that should be
taken as \code{NA}. Set to \code{NULL} to turn off this feature.}

\item{type}{optional type of CTD, e.g. "SBE"}

\item{serialNumber}{optional serial number of instrument}

\item{ship}{optional string containing the ship from which the observations were made.}

\item{cruise}{optional string containing a cruise identifier.}

\item{station}{optional string containing a station identifier.}

\item{startTime}{optional indication of the start time for the profile,
which is used in some several plotting functions.  This is best given as a
\link{POSIXt} time, but it may also be a character string
that can be converted to a time with \code{\link[=as.POSIXct]{as.POSIXct()}},
using \code{UTC} as the timezone.}

\item{longitude}{optional numerical value containing longitude in decimal
degrees, positive in the eastern hemisphere. If this is a single number,
then it is stored in the \code{metadata} slot of the returned value; if it
is a vector of numbers, then they are stored in the \code{data} slot.}

\item{latitude}{optional numerical value containing the latitude in decimal
degrees, positive in the northern hemisphere. See the note on length, for
the \code{longitude} argument.}

\item{deploymentType}{character string indicating the type of deployment. Use
\code{"unknown"} if this is not known, \code{"profile"} for a profile (in
which the data were acquired during a downcast, while the device was lowered
into the water column, perhaps also including an upcast; \code{"moored"} if
the device is installed on a fixed mooring, \code{"thermosalinograph"} (or
\code{"tsg"}) if the device is mounted on a moving vessel, to record
near-surface properties, or \code{"towyo"} if the device is repeatedly
lowered and raised.}

\item{pressureAtmospheric}{A numerical value (a constant or a vector),
that is subtracted from pressure before storing it in the return value.
(This altered pressure is also used in calculating \code{salinity}, if
that is to be computed from \code{conductivity}, etc., using
\code{\link[=swSCTp]{swSCTp()}}; see \code{salinity} above.)}

\item{sampleInterval}{optional numerical value indicating the time between
samples in the profile.}

\item{profile}{optional positive integer specifying the number of the profile
to extract from an object that has data in matrices, such as for some
\code{argo} objects. Currently the \code{profile} argument is only utilized for
\linkS4class{argo} objects.}

\item{debug}{an integer specifying whether debugging information is
to be printed during the processing. This is a general parameter that
is used by many \code{oce} functions. Generally, setting \code{debug=0}
turns off the printing, while higher values suggest that more information
be printed. If one function calls another, it usually reduces the value of
\code{debug} first, so that a user can often obtain deeper debugging
by specifying higher \code{debug} values.}
}
\value{
A \linkS4class{ctd} object.
}
\description{
Assemble data into a \linkS4class{ctd} object.  This function is complicated
(spanning approximately 500 lines of code) because it tries to handle many
special cases, and tries to make sensible defaults for unspecified
parameters.  If odd results are found, users might find it helpful to call
this function with the first argument being a simple vector of Practical
Salinity values, in which case the processing of the other arguments is
relatively straightforward.
}
\details{
The following sections provide an some notes on how \code{as.ctd()} handles
certain object types given as the first parameter.

\strong{Converting argo objects}

If the \code{salinity} argument is an object of \linkS4class{argo}, then that
object is dismantled and reassembled as a \linkS4class{ctd} object in ways that
are mostly straightforward, although the handling of time depends
on the information in the original netcdf data file that was used
by \code{\link[=read.argo]{read.argo()}} to create the \linkS4class{argo} object.

All Argo data files contain an item called \code{juld} from which the profile
time can be computed, and some also contain an additional item named \code{MTIME},
from which the times of individual measurements can also be computed.  Both
cases are handled by \code{as.ctd()}, using a scheme outlined in
Note 4 of the Details section of the \code{\link[=read.argo]{read.argo()}} documentation.

\strong{Converting rsk objects}

If the \code{salinity} argument is an object of \linkS4class{rsk},
then \code{as.ctd} passes it,
\code{pressureAtmospheric},
\code{longitude},
\code{latitude}
\code{ship},
\code{cruise},
\code{station} and
\code{deploymentType}
to \code{\link[=rsk2ctd]{rsk2ctd()}}, which builds the ctd object that is
returned by \code{as.ctd}. The other arguments to \code{as.ctd}
are ignored in this instance, because \code{rsk} objects already
contain their information. If required, any data or metadata
element can be added to the value returned by \code{as.ctd}
using \code{\link[=oceSetData]{oceSetData()}} or \code{\link[=oceSetMetadata]{oceSetMetadata()}},
respectively.

The returned \linkS4class{rsk} object contains pressure in a form that
may need to be adjusted, because \code{rsk} objects may contain
either absolute pressure or sea pressure. This adjustment is handled
automatically by \code{as.ctd}, by examination of the metadata item
named \code{pressureType} (described in the documentation for
\code{\link[=read.rsk]{read.rsk()}}).  Once the sea pressure is determined,
adjustments may be made with the \code{pressureAtmospheric} argument,
although in that case it is better considered a pressure adjustment
than the atmospheric pressure.

\linkS4class{rsk} objects may store sea pressure or absolute pressure (the
sum of sea pressure and atmospheric pressure), depending on how the object was
created with \code{\link[=as.rsk]{as.rsk()}} or \code{\link[=read.rsk]{read.rsk()}}.  However,
\linkS4class{ctd} objects store sea pressure, which is needed for
plotting, calculating density, etc. This poses no difficulties, however,
because \code{as.ctd} automatically converts absolute pressure to sea pressure,
if the metadata in the \linkS4class{rsk} object indicates that this is
appropriate. Further alteration of the pressure can be accomplished with the
\code{pressureAtmospheric} argument, as noted above.
}
\examples{
library(oce)
# 1. fake data, with default units
pressure <- 1:50
temperature <- 10 - tanh((pressure - 20) / 5) + 0.02 * rnorm(50)
salinity <- 34 + 0.5 * tanh((pressure - 20) / 5) + 0.01 * rnorm(50)
ctd <- as.ctd(salinity, temperature, pressure)
# Add a new column
fluo <- 5 * exp(-pressure / 20)
ctd <- oceSetData(ctd,
    name = "fluorescence", value = fluo,
    unit = list(unit = expression(mg / m^3), scale = "")
)
summary(ctd)

# 2. fake data, with supplied units (which are the defaults, actually)
ctd <- as.ctd(salinity, temperature, pressure,
    units = list(
        salinity = list(unit = expression(), scale = "PSS-78"),
        temperature = list(unit = expression(degree * C), scale = "ITS-90"),
        pressure = list(unit = expression(dbar), scale = "")
    )
)

}
\references{
Culkin, F., and Norman D. Smith, 1980. Determination of the
concentration of potassium chloride solution having the same electrical
conductivity, at 15 C and infinite frequency, as standard seawater of salinity
35.0000 ppt (Chlorinity 19.37394 ppt). \emph{IEEE Journal of Oceanic
Engineering}, volume \strong{5}, pages 22-23.
}
\seealso{
Other things related to ctd data: 
\code{\link{CTD_BCD2014666_008_1_DN.ODF.gz}},
\code{\link{[[,ctd-method}},
\code{\link{[[<-,ctd-method}},
\code{\link{cnvName2oceName}()},
\code{\link{ctd}},
\code{\link{ctd-class}},
\code{\link{ctd.cnv.gz}},
\code{\link{ctdDecimate}()},
\code{\link{ctdFindProfiles}()},
\code{\link{ctdFindProfilesRBR}()},
\code{\link{ctdRaw}},
\code{\link{ctdRepair}()},
\code{\link{ctdTrim}()},
\code{\link{ctd_aml.csv.gz}},
\code{\link{d200321-001.ctd.gz}},
\code{\link{d201211_0011.cnv.gz}},
\code{\link{handleFlags,ctd-method}},
\code{\link{initialize,ctd-method}},
\code{\link{initializeFlagScheme,ctd-method}},
\code{\link{oceNames2whpNames}()},
\code{\link{oceUnits2whpUnits}()},
\code{\link{plot,ctd-method}},
\code{\link{plotProfile}()},
\code{\link{plotScan}()},
\code{\link{plotTS}()},
\code{\link{read.ctd}()},
\code{\link{read.ctd.aml}()},
\code{\link{read.ctd.itp}()},
\code{\link{read.ctd.odf}()},
\code{\link{read.ctd.odv}()},
\code{\link{read.ctd.saiv}()},
\code{\link{read.ctd.sbe}()},
\code{\link{read.ctd.ssda}()},
\code{\link{read.ctd.woce}()},
\code{\link{read.ctd.woce.other}()},
\code{\link{setFlags,ctd-method}},
\code{\link{subset,ctd-method}},
\code{\link{summary,ctd-method}},
\code{\link{woceNames2oceNames}()},
\code{\link{woceUnit2oceUnit}()},
\code{\link{write.ctd}()}
}
\author{
Dan Kelley
}
\concept{things related to ctd data}
