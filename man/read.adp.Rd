\name{read.adp}
\alias{read.adp}
\alias{read.adp.nortek}
\alias{read.adp.sontek}
\alias{read.adp.sontek.serial}
\alias{read.adp.rdi}
\alias{read.aquadopp}
\alias{read.aquadoppProfiler}
\alias{read.aquadoppHR}

\title{Read an ADP data file}

\description{Read an ADP data file, producing an object of type \code{adp}.}

\usage{
read.adp(file, from=1, to, by=1, tz=getOption("oceTz"),
  latitude=NA, longitude=NA,
  manufacturer=c("rdi", "nortek", "sontek"),
  monitor=FALSE, despike=FALSE, processingLog,
  debug=getOption("oceDebug"),
  \dots)
read.adp.rdi(file, from=1, to, by=1, tz=getOption("oceTz"),
  latitude=NA, longitude=NA,
  type=c("workhorse"),
  monitor=FALSE, despike=FALSE, processingLog,
  testing=FALSE,
  debug=getOption("oceDebug"),
  \dots)

read.aquadopp(file, from=1, to, by=1, tz=getOption("oceTz"),
  latitude=NA, longitude=NA,
  orientation, distance,
  monitor=FALSE, despike=FALSE, processingLog,
  debug=getOption("oceDebug"),
  \dots)
read.aquadoppHR(file, from=1, to, by=1, tz=getOption("oceTz"),
  latitude=NA, longitude=NA,
  orientation, distance,
  monitor=FALSE, despike=FALSE, processingLog,
  debug=getOption("oceDebug"),
  \dots)
read.aquadoppProfiler(file, from=1, to, by=1, tz=getOption("oceTz"),
  latitude=NA, longitude=NA,
  orientation, distance,
  monitor=FALSE, despike=FALSE, processingLog,
  debug=getOption("oceDebug"),
  \dots)

read.adp.nortek(file, from=1, to, by=1, tz=getOption("oceTz"),
  latitude=NA, longitude=NA,
  type=c("aquadoppHR", "aquadoppProfiler", "aquadopp"),
  orientation, distance,
  monitor=FALSE, despike=FALSE, processingLog,
  debug=getOption("oceDebug"),
  \dots)

read.adp.sontek(file, from=1, to, by=1, tz=getOption("oceTz"),
  latitude=NA, longitude=NA,
  type=c("adp","pcadp"),
  monitor=FALSE, despike=FALSE, processingLog,
  debug=getOption("oceDebug"),
  \dots)
read.adp.sontek.serial(file, from=1, to, by=1, tz=getOption("oceTz"),
  latitude=NA, longitude=NA,
  type=c("adp", "pcadp"),
  beamAngle=25, orientation,
  monitor=FALSE, processingLog,
  debug=getOption("oceDebug")) 
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
  \item{manufacturer}{a character string indicating the manufacturer, used 
    by the general function \code{read.adp} to select a subsidiary 
   function to use, such as \code{read.adp.nortek}.}
  \item{type}{a character string indicating the type of instrument.}
  \item{orientation}{optional character string specifying the orientation of
      the sensor, provided for those cases in which it cannot be inferred from
      the data file.  The valid choices are \code{"upward"}, \code{"downward"},
      and \code{"sideward"}.}
  \item{distance}{optional vector holding the distances of bin centres from the
      sensor.  This argument is ignored except for Nortek profilers, and need
      not be given if the function determines the distances correctly from the
      data.  The problem is that the distance is poorly documented in the
      Nortek System Integrator Guide (2008 edition, page 31), so the function
      must rely on word-of-mouth formulae that do not work in all cases.}
  \item{tz}{character string indicating time zone to be assumed in the data.}
  \item{latitude}{optional signed number indicating the latitude in 
    degrees North.}
  \item{longitude}{optional signed number indicating the longitude in 
    degrees East.}
  \item{beamAngle}{angle between instrument axis and beams, in degrees.}
  \item{monitor}{boolean, set to \code{TRUE} to provide an indication
    (with numbers and dots) of every profile read.}
  \item{despike}{if \code{TRUE}, \code{\link{despike}} will be used
    to clean anomalous spikes in heading, etc.}
  \item{processingLog}{if provided, the action item to be stored in the
    log.  (Typically only provided for internal calls; the default that
    it provides is better for normal calls by a user.)}
  \item{testing}{a flag that applies only to RDI units.  If this is
      \code{TRUE}, then the time-varying device orientation is inferred from
      the per-profile header information, and the boolean result is stored in
      an integer vector named \code{upward} within the \code{data} slot.}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
    moderate amount of debugging information, or to 2 to get more.} 
  \item{\dots}{additional arguments, passed to called routines.}
}

\details{Reads a binary-format ADP file.  Several file types can be handled.
    Some of these functions are wrappers that map to device names, e.g.
    \code{read.aquadoppProfiler} does its work by calling
    \code{read.adp.nortek}; in this context, it is worth noting that the
    ``aquadopp'' instrument is a one-cell profiler that might just as well have
    been documented under the heading \code{\link{read.adv}}.}

\value{An object of \code{\link[base]{class}} \code{"adp"}, which
  contains measurements made with an ADP device.  The value of
  \code{metadata$coordinate} is set to \code{"beam"}, a fact that is
  used in other steps in processing.  For information on data stored in
  the object, see \dQuote{Details}.

  There are three types of element stored in the result's \code{data},
  namely space-series, time-series, and matrix.  These are contained
  within a list named \code{data}, as follows:
  \describe{
    \item{\code{distance}}{A space-series that 
      stores the distance of cells from the transducer head, in the
      vertical (not slant-wise) direction.}
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
  \item{\code{v}}{A 3-D matrix of velocity.  The first index corresponds to
      profile number, the second to cell number, and the third to beam number.}
  \item{\code{a}}{A 3-D matrix of backscatter amplitude, corresponding to \code{v}.}
  \item{\code{q}}{A 3-D matrix of a measure of the quality of the data,
      corresponding to \code{v}.}
  \item{\code{br}}{Depth to bottom in each of 4 beams (only for Teledyne-RDI
      devices that have bottom tracking.}
  \item{\code{bv}}{Bottom velocity in each of 4 beams (only for Teledyne-RDI
      devices that have bottom tracking.}
  }

  In ADP data files, velocities are coded to signed 2-byte integers, with a
  scale factor being used to convert to velocity in metres per second.  These
  two facts control the maximum recordable velocity and the velocity
  resolution, values that may be retrieved for an ADP object name \code{d} with
  \code{d[["velocityMaximum"]]} and \code{d[["velocityResolution"]]}.

}

\section{Implementation notes}{
  \itemize{
    \item \strong{Teledyne-RDI files.} If a heading bias had been set with the
    \code{EB} command during the setup for the deployment, then a heading bias
    will have been stored in the file's header.  This value is stored in the
    object's metadata as \code{metadata$heading.bias}.  \strong{Importantly},
    this value is subtracted from the headings stored in the file, and the
    result of this subtraction is stored in the objects heading value (in
    \code{data$heading}).  It should be noted that \code{read.adp.rdi()} was
    tested for firmware version 16.30.  For other versions, there may be
    problems.  For example, the serial number is not recognized properly for
    version 16.28.
    \item \strong{Nortek Aquadopp files.} The R code is based on information in
    the Nortek System Integrator Guide (2008) and on postings on the Nortek
    ``knowledge center'' discussion board.  One might assume that the latter is
    less authorative than the former.  For example, the inference of cell size
    follows advice found at
    \url{http://www.nortekusa.com/en/knowledge-center/forum/hr-profilers/736804717}
    (downloaded June 2012)), which contains a typo in an early posting that is
    corrected later on.}
}

\seealso{The documentation for \code{\link{adp-class}} explains the structure
    of ADP objects, and also outlines the other functions dealing with them.}

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

2. Information on Nortek profilers (including the System Integrator Guide,
which explains the data format byte-by-byte) is available at
\url{http://www.nortekusa.com/}.  (One must join the site to see the manuals.)

3. Information about Sontek profilers is available at
\url{http://www.sontek.com}.

4. The Nortek Knowledge Center
\url{http://www.nortekusa.com/en/knowledge-center} may be of help if problems
arise in dealing with data from Nortek instruments.

}

\author{Dan Kelley}

\keyword{misc}
