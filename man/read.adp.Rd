\name{read.adp}
\alias{read.adp}
\alias{read.adp.nortek}
\alias{read.adp.sontek}
\alias{read.adp.sontek.serial}
\alias{read.adp.rdi}

\title{Read an ADP data file}

\description{Read an ADP data file, producing an object of type \code{adp}.}

\usage{
read.adp(file, from=1, to, by=1, tz=getOption("oce.tz"),
  latitude=NA, longitude=NA,
  type=c("rdi", "nortek", "sontek"),
  debug=getOption("oce.debug"), monitor=TRUE, despike=FALSE, log.action,
  \dots)
read.adp.rdi(file, from=1, to, by=1, tz=getOption("oce.tz"),
  latitude=NA, longitude=NA,
  type=c("workhorse"),
  debug=getOption("oce.debug"), monitor=TRUE, despike=FALSE, log.action,
  \dots)
read.adp.nortek(file, from=1, to, by=1, tz=getOption("oce.tz"),
  latitude=NA, longitude=NA,
  type=c("aquadopp high resolution"),
  debug=getOption("oce.debug"), monitor=TRUE, despike=FALSE, log.action,
  \dots)
read.adp.sontek(file, from=1, to, by=1, tz=getOption("oce.tz"),
  latitude=NA, longitude=NA,
  type=c("adp","pcadp"),
  debug=getOption("oce.debug"), monitor=TRUE, despike=FALSE, log.action,
  \dots)
read.adp.sontek.serial(file, from=1, to, by=1, tz=getOption("oce.tz"),
                      latitude=NA, longitude=NA,
                      monitor=TRUE, log.action,
                      debug=getOption("oce.debug")) 
}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to load.  (For \code{read.adp.sontek.serial}, this is
    generally a list of files, which will be concatenated.}
  \item{from}{indication of the first profile to read.  This can be an
    integer, the sequence number of the first profile to read, or a
    POSIXt time before which profiles should be skipped, or a character
    string that converts to a POSIXt time (assuming UTC timezone).  See
    \dQuote{Examples}, and make careful note of the use of the \code{tz}
    argument.}
  \item{to}{if supplied, an indication of the last profile to read, in a
    format as described for \code{from}.  If not supplied, the whole
    file will be read.}
  \item{by}{an indication of the stride length to use while walking
    through the file.  If this is an integer, then \code{by-1}
    profiles are skipped between each pair of profiles that is read.  If
    this is a string representing a time interval, in colon-separated
    format (MM:SS), then this interval is divided by the sampling interval, to
    get the stride length.}
  \item{type}{a character string indicating th type of file.}
  \item{tz}{character string indicating time zone to be assumed in the data.}
  \item{latitude}{optional signed number indicating the latitude in 
    degrees North.}
  \item{longitude}{optional signed number indicating the longitude in 
    degrees East.}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
    moderate amount of debugging information, or to 2 to get more.} 
  \item{monitor}{boolean, set to \code{TRUE} to provide an indication
    (with numbers and dots) of every profile read.}
  \item{despike}{if \code{TRUE}, \code{\link{despike}} will be used
    to clean anomalous spikes in heading, etc.}
  \item{log.action}{if provided, the action item to be stored in the
    log.  (Typically only provided for internal calls; the default that
    it provides is better for normal calls by a user.)}
  \item{\dots}{additional arguments, passed to called routines.}
}

\details{Reads a binary-format ADP file.  Three types can be handled at
  the moment: the Teledyne-RDI Workhorse instrument using firmware
  version 16.30 (and, to some extent, also firmware 16.28 and 16.21),
  the NorTek Aquadopp High Resolution profiler, and SonTek PCADP.}

\value{An object of \code{\link[base]{class}} \code{"adp"}, which
  contains measurements made with an ADP device.  The value of
  \code{metadata$coordinate} is set to \code{"beam"}, a fact that is
  used in other steps in processing.  For information on data stored in
  the object, see \dQuote{Details}.

  There are three types of element stored in the result's \code{data},
  namely space-series, time-series, and matrix.  These are contained
  within lists named \code{data$ss}, \code{data$ts} and \code{data$ma},
  respectively, with contents as follows.
  \describe{
    \item{\code{ss}}{A space-series named \code{distance}, which
      stores the distance of cells from the transducer head, in the
      vertical (not slant-wise) direction.}
    \item{\code{ts}}{A collection time series, each of which has a value
      for each profile.  The series are as follows.
      	\describe{
	  \item{\code{time}}{Times of the profiles, in POSIXct format.}
	  \item{\code{pressure}}{Pressure, in decibars.}
	  \item{\code{temperature}}{The temperature, in deg C.}
	  \item{\code{salinity}}{The salinity, in PSU.  (This may be
            measured, or just a repeated constant value specified when
            the equipment was set up.)}
	  \item{\code{depth.of.transducer}}{The depth of the
            transducer.}
	  \item{\code{heading}}{The heading of the instrument, in
            degrees.}
	  \item{\code{pitch}}{The pitch of the instrument, in degrees.}
	  \item{\code{roll}}{The roll of the instrument, in degrees.}
	}}
    \item{\code{ma}}{A set of 3D arrays that contain the profile data,
      as follows.
      \describe{
        \item{\code{v}}{The velocity.}
	\item{\code{a}}{The amplitude of backscatter.}
	\item{\code{q}}{A measure of the quality of the data,
   	  e.g. correlation or percent-good.}
      }
      The first index corresponds to profile number, the second index
      to cell number (distance from sensor), and the third to beam
      number.  Objects of class \code{rdi} typically have 4 beams, while
      those of type \code{aquadopp} have 3 beams.}
    }
    In addition to the \code{data} entry, the \code{metadata} entry
    holds general information about such things as beam geometry, etc.}

\section{Implementation notes}{
  \itemize{
    \item \strong{Teledyne-RDI files.} If a heading bias had been set
    with the \code{EB} command during the setup for the deployment, then
    a heading bias will have been stored in the file's header.  This
    value is stored in the object's metadata as
    \code{metadata$heading.bias}.  \strong{Importantly}, this value is
    subtracted from the headings stored in the file, and the result of
    this subtraction is stored in the objects heading value (in
    \code{data$ts$heading}).
    
    It should be noted that \code{read.adp.rdi()} was tested for
    firmware version 16.30.  For other versions, there may be problems.
    For example, the serial number is not recognized properly for
    version 16.28.
    
    \item \strong{Nortek Aquadopp-HR files.} Page 38 of the Nortek
    System Integrator Guide says to offset 53 bytes in profile records
    to get the first velocity, but experiments on files drived from a
    Dalhousie University profiler require an offset of 54 to recover
    data that match the manufacturer's (*.v1, *.v2, *.v3) files.  Also,
    the beam angle is hard-wired at 25 degrees in the code, since it
    is not documented as being part of the headers.  (Actually,
    the beam angle is in the header, in bytes 23 to 30 of the
    beam-header, but it is not documented, and sample code that Nortek
    provided to me in 2009 gives the scale incorrectly, as compared
    with files created with a Dalhousie aquadopHR.)}
}

\seealso{The generic function \code{\link{read.oce}} provides an
  alternative to this. An \code{adp} object may be summarized with
  \code{\link{summary.adp}}, and plotted with \code{\link{plot.adp}}.

  Several functions are provided to manipulate \code{adp} objects.  For
  example, in many research applications, the records are stored in beam
  coordinates, instead of the further-processed forms such as instrument
  or enu coordinates.  Accordingly, \code{\link{adp.beam2xyz}} is
  provided, convert the velocity portions of \code{adp} objects from
  beam coordinates to xyz-based (also called ship-based or
  instrument-based) coordinates.  Similarly,
  \code{\link{adp.xyz2enu}} converts from xyz to enu
  coordinates, taking into account the varying orientation of an
  instrument attached to a ship or a mobile mooring.  For applications
  in which enu-based coordinates are not desired (e.g. if a coordinate
  is to be aligned with the local coastline or a mean current),
  \code{\link{adp.enu2other}} is provided, to convert from enu
  coordinates to another coordinate system. 

 Another convenience function is \code{\link{adp.beam.attenuate}},
which performs a correction for \eqn{r^2}{r^2} beam spreading.}

\examples{
\dontrun{
library(oce)
# A day sampled at 1/2 hour interval.  Note the timezone.
dir <- "/data/archive/sleiwex/2008/moorings/"
f <- paste(dir, "m09/adp/rdi_2615/raw/adp_rdi_2615.000", sep="")
d <- read.oce(f, from=as.POSIXct("2008-06-26", tz="UTC"),
                 to=as.POSIXct("2008-06-27", tz="UTC"), by="30:00")
summary(d)
plot(d)
}
}

\references{1. Teledyne-RDI, 2007. \emph{WorkHorse commands and output data
    format.} P/N 957-6156-00 (November 2007).  
(Section 5.3 h details the binary format, e.g.
the file should start with the byte \code{0x7f} repeated twice, 
and each profile starts with the bytes
\code{0x80}, followed by \code{0x00}, followed by the
sequence number of the profile, represented as a little-endian
two-byte short integer.  \code{read.adp.rdi()} uses
these sequences to interpret data files.)

2. Information on Nortek profiler is available at
\url{http://www.nortekusa.com/}.  (One must join the site to see the
manuals.)

3. Information about Sontek profilers is available at
\url{http://www.sontek.com}.
}

\author{Dan Kelley}

\keyword{misc}
