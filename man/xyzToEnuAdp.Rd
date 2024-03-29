% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adp.R
\name{xyzToEnuAdp}
\alias{xyzToEnuAdp}
\title{Convert adp Object From XYZ to ENU Coordinates}
\usage{
xyzToEnuAdp(x, declination = 0, debug = getOption("oceDebug"))
}
\arguments{
\item{x}{an \linkS4class{adp} object.}

\item{declination}{magnetic declination to be added to the heading after
"righting" (see below), to get ENU with N as "true" north.  If this
is set to NULL, then the returned object is set up without adjusting
the compass for declination.  That means that \code{north} in its \code{metadata}
slot will be set to \code{"magnetic"}, and also that there will be no item
named \code{declination} in that slot.  Note that \code{\link[=applyMagneticDeclination]{applyMagneticDeclination()}}
can be used later, to set a declination.}

\item{debug}{an integer specifying whether debugging information is
to be printed during the processing. This is a general parameter that
is used by many \code{oce} functions. Generally, setting \code{debug=0}
turns off the printing, while higher values suggest that more information
be printed. If one function calls another, it usually reduces the value of
\code{debug} first, so that a user can often obtain deeper debugging
by specifying higher \code{debug} values.}
}
\value{
An object with \code{data$v[,,1:3]} altered appropriately, and
\code{x[["oceCoordinate"]]} changed from \code{xyz} to \code{enu}.
}
\description{
Convert ADP velocity components from a xyz-based coordinate system to
an enu-based coordinate system, by using the instrument's recording of
information relating to heading, pitch, and roll. The action is based
on what is stored in the data, and so it depends greatly on instrument type
and the style of original data format. This function handles data from
RDI Teledyne, Sontek, and some Nortek instruments directly.
}
\details{
The first step is to convert the (x,y,z) velocity components (stored in the
three columns of \code{x[["v"]][,,1:3]}) into what RDI (reference 1, pages 11 and 12)
calls "ship" (or "righted") components.  For example, the z coordinate,
which may point upwards or downwards depending on instrument orientation, is
mapped onto a "mast" coordinate that points more nearly upwards than
downward.  The other ship coordinates are called "starboard" and "forward",
the meanings of which will be clear to mariners.  Once the (x,y,z)
velocities are converted to ship velocities, the orientation of the
instrument is extracted from heading, pitch, and roll vectors stored in the
object.  These angles are defined differently for RDI and Sontek profilers.

The code handles every case individually, based on the table given below.
The table comes from Clark Richards, a former PhD student at Dalhousie
University (reference 2), who developed it based on instrument documentation,
discussion on user groups, and analysis of measurements acquired with RDI
and Sontek acoustic current profilers in the SLEIWEX experiment.  In the
table, (X, Y, Z) denote instrument-coordinate velocities, (S, F, M) denote
ship-coordinate velocities, and (H, P, R) denote heading, pitch, and roll.

\tabular{rrrrrrrrrrrr}{ \strong{Case} \tab \strong{Mfr.} \tab
\strong{Instr.} \strong{Orient.} \tab \strong{H} \tab \strong{P} \tab
\strong{R} \tab \strong{S} \tab \strong{F} \tab \strong{M}\cr 1 \tab RDI
\tab ADCP \tab up \tab H \tab arctan(tan(P)*cos(R)) \tab R \tab -X \tab Y
\tab -Z\cr 2 \tab RDI \tab ADCP \tab down \tab H \tab arctan(tan(P)*cos(R))
\tab -R \tab X \tab Y \tab Z\cr 3 \tab Nortek \tab ADP \tab up \tab H-90
\tab R \tab -P \tab X \tab Y \tab Z\cr 4 \tab Nortek \tab ADP \tab down \tab
H-90 \tab R \tab -P \tab X \tab -Y \tab -Z\cr 5 \tab Sontek \tab ADP \tab up
\tab H-90 \tab -P \tab -R \tab X \tab Y \tab Z\cr 6 \tab Sontek \tab ADP
\tab down \tab H-90 \tab -P \tab -R \tab X \tab Y \tab Z\cr 7 \tab Sontek
\tab PCADP \tab up \tab H-90 \tab R \tab -P \tab X \tab Y \tab Z\cr 8 \tab
Sontek \tab PCADP \tab down \tab H-90 \tab R \tab -P \tab X \tab Y \tab Z\cr
}

Finally, a standardized rotation matrix is used to convert from ship
coordinates to earth coordinates.  As described in the RDI coordinate
transformation manual (reference 1, pages 13 and 14), this matrix is based on sines
and cosines of heading, pitch, and roll If \code{CH} and \code{SH} denote
cosine and sine of heading (after adjusting for declination), with similar
terms for pitch and roll using second letters \code{P} and \code{R}, the
rotation matrix is

\preformatted{ rbind(c( CH*CR + SH*SP*SR, SH*CP, CH*SR - SH*SP*CR), c(-SH*CR
+ CH*SP*SR, CH*CP, -SH*SR - CH*SP*CR), c( -CP*SR, SP, CP*CR)) }

This matrix is left-multiplied by a matrix with three rows, the top a vector
of "starboard" values, the middle a vector of "forward" values, and the
bottom a vector of "mast" values.  Finally, the columns of
\code{data$v[,,1:3]} are filled in with the result of the matrix
multiplication.
}
\references{
\enumerate{
\item Teledyne RD Instruments. \dQuote{ADCP Coordinate Transformation: Formulas and Calculations,}
January 2010. P/N 951-6079-00.
\item Clark Richards, 2012, PhD Dalhousie University Department of
Oceanography.
}
}
\seealso{
Other things related to adp data: 
\code{\link{[[,adp-method}},
\code{\link{[[<-,adp-method}},
\code{\link{ad2cpCodeToName}()},
\code{\link{ad2cpHeaderValue}()},
\code{\link{adp}},
\code{\link{adp-class}},
\code{\link{adpAd2cpFileTrim}()},
\code{\link{adpConvertRawToNumeric}()},
\code{\link{adpEnsembleAverage}()},
\code{\link{adpFlagPastBoundary}()},
\code{\link{adpRdiFileTrim}()},
\code{\link{adp_rdi.000}},
\code{\link{applyMagneticDeclination,adp-method}},
\code{\link{as.adp}()},
\code{\link{beamName}()},
\code{\link{beamToXyz}()},
\code{\link{beamToXyzAdp}()},
\code{\link{beamToXyzAdpAD2CP}()},
\code{\link{beamToXyzAdv}()},
\code{\link{beamUnspreadAdp}()},
\code{\link{binmapAdp}()},
\code{\link{enuToOther}()},
\code{\link{enuToOtherAdp}()},
\code{\link{handleFlags,adp-method}},
\code{\link{is.ad2cp}()},
\code{\link{plot,adp-method}},
\code{\link{read.adp}()},
\code{\link{read.adp.ad2cp}()},
\code{\link{read.adp.nortek}()},
\code{\link{read.adp.rdi}()},
\code{\link{read.adp.sontek}()},
\code{\link{read.adp.sontek.serial}()},
\code{\link{read.aquadopp}()},
\code{\link{read.aquadoppHR}()},
\code{\link{read.aquadoppProfiler}()},
\code{\link{rotateAboutZ}()},
\code{\link{setFlags,adp-method}},
\code{\link{subset,adp-method}},
\code{\link{subtractBottomVelocity}()},
\code{\link{summary,adp-method}},
\code{\link{toEnu}()},
\code{\link{toEnuAdp}()},
\code{\link{velocityStatistics}()},
\code{\link{xyzToEnu}()},
\code{\link{xyzToEnuAdpAD2CP}()}
}
\author{
Dan Kelley and Clark Richards
}
\concept{things related to adp data}
