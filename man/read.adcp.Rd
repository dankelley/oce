\name{read.adcp}
\alias{read.adcp}
\alias{read.adcp.nortek}
\alias{read.adcp.sontek}
\alias{read.adcp.rdi}

\title{Read an ADCP data file}

\description{Read an ADCP data file, producing an object of type \code{adcp}.}

\usage{
read.adcp(file, from=0, to, by=1,
          type=c("rdi", "nortek"),
          debug=0, monitor=TRUE, log.action)
read.adcp.rdi(file, from=0, to, by=1,
              type=c("workhorse"),
              debug=0, monitor=TRUE, log.action)
read.adcp.nortek(file, from=0, to, by=1,
                 type=c("aquadopp high resolution"),
                 debug=0, monitor=TRUE, log.action)
read.adcp.sontek(file, from=0, to, by=1,
                 type=c("adp"),
                 withHeader=FALSE,
                 debug=0, monitor=TRUE, log.action)
}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to load.}
  \item{from}{number of profiles to skip at start, or the POSIXt time at
    which to start reading.  See \dQuote{Examples}, and make careful
    note of the use of the \code{tz} argument.}
  \item{to}{optional number of profiles to read, or the last POSIXt
    time to read.}
  \item{by}{an indication of the stride length to use while walking
    through the file.  If this is an integer, then \code{by-1}
    profiles are skipped between each pair of profiles that is read.  If
    this is a string representing a time interval, in colon-separated
    format, then this interval is divided by the sampling interval, to
    get the stride length.}
  \item{type}{a character string indicating th type of file.}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
    moderate amount of debugging information, or to 2 to get more.} 
  \item{monitor}{boolean, set to \code{TRUE} to provide an indication
    (with numbers and dots) of every profile read.}
  \item{log.action}{if provided, the action item to be stored in the
    log.  (Typically only provided for internal calls; the default that
    it provides is better for normal calls by a user.)}
}

\details{Reads a binary-format ADCP file.  Two types can be handled at
  the moment: the Teledyne-RDI (2007) Workhorse instrument using
  firmware version 16.28, and the NorTek Aquadopp High Resolution
  profiler.  \code{read.adcp} may also work on other  RDI
  units, but this has not been tested.  It definitely does not work on
  other NorTek units, because the binary format is different for each.

  There are three types of element stored in the result's \code{data},
  namely space-series, time-series, and matrix.  These are contained
  within lists named \code{data$ss}, \code{data$ts} and \code{data$ma},
  and their contents are as follows.
  \describe{
    \item{\code{ss}}{A space-series named \code{distance}, which
      stores the distance of cells from the transducer head, in the
      vertical (not slant-wise) direction.}
    \item{\code{ts}}{A collection time series, each of which has a value for each
      profile.  The series are as follows.
      	\describe{
	  \item{\code{time}}{Times of the profiles, in POSIXct format.}
	  \item{\code{pressure}}{Pressure, in decibars.}
	  \item{\code{temperature}}{The temperature, in deg C.}
	  \item{\code{salinity}}{The salinity, in PSU.  (This may be measured, or 
	  just a repeated constant value specified when the equipment was set up.)}
	  \item{\code{depth.of.transducer}}{The depth of the transducer.}
	  \item{\code{heading}}{The heading of the instrument, in degrees.}
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
    In addition to the \code{data} entry, the \code{metadata} entry holds
    general information about such things as beam geometry, etc.
}

\value{An object of \code{\link[base]{class}} \code{"adcp"}, which
  contains measurements made with an ADCP device.  The value of
  \code{metadata$coordinate} is set to \code{"beam"}, a fact that is
  used in other steps in processing.  For information on data stored in
  the object, see \dQuote{Details}.}


\seealso{The generic function \code{\link{read.oce}} provides an
  alternative to this. An \code{adcp} object may be summarized with
  \code{\link{summary.adcp}}, and plotted with \code{\link{plot.adcp}}.

  Several functions are provided to manipulate \code{adcp} objects.  For
  example, in many research applications, the records are stored in beam
  coordinates, instead of the further-processed forms such as instrument
  or earth coordiantes.  Accordingly, \code{\link{adcp.beam2frame}} is
  provided, convert the velocity portions of \code{adcp} objects from
  beam coordinates to frame-based (also called ship-based or
  instrument-based) coordinates.  Similarly,
  \code{\link{adcp.frame2earth}} converts from frame to earth
  coordinates, taking into account the varying orientation of an
  instrument attached to a ship or a mobile mooring.  For applications
  in which earth-based coordinates are not desired (e.g. if a coordinate
  is to be aligned with the local coastline or a mean current),
  \code{\link{adcp.earth2other}} is provided, to convert from earth
  coordinates to another coordinate system. 

 Another convenience
  function is \code{\link{adcp.beam.attenuate}}, which performs a
  correction for \eqn{r^2}{r^2} beam spreading.}

\examples{
\dontrun{
library(oce)
# A day sampled at 1/2 hour interval.  Note the timezone.
d <- read.oce("~/SL08F001.000", 
               from=as.POSIXct("2008-06-26", tz="UTC"),
               to=as.POSIXct("2008-06-27", tz="UTC"), 
               by="30:00")
summary(d)
plot(d)
}
}

\references{Teledyne-RDI, 2007. \emph{WorkHorse commands and output data
    format.} P/N 957-6156-00 (November 2007).

Information on the Nortek profiler is available at
\url{http://www.nortekusa.com/}.  (One must join the site to see the
manuals, so no citation is provided here.)}

\author{Dan Kelley}

\keyword{misc}
