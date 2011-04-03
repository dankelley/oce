\name{read.adv}
\alias{read.adv}
\alias{read.adv.nortek}
\alias{read.adv.sontek.serial}
\alias{read.adv.sontek.adr}
\alias{read.adv.sontek.text}

\title{Read an ADV data file}

\description{Read an ADV data file, producing an object of type \code{adv}.}

\usage{
read.adv(file, from=1, to, by=1, tz=getOption("oce.tz"),
  type=c("nortek", "sontek", "sontek.adr", "sontek.text"),
  header=TRUE,
  latitude=NA, longitude=NA,
  start, deltat,
  debug=getOption("oce.debug"), monitor=TRUE, log.action)
read.adv.nortek(file, from=1, to, by=1, tz=getOption("oce.tz"), 
  type="vector",
  header=TRUE,
  latitude=NA, longitude=NA,
  debug=getOption("oce.debug"), monitor=TRUE, log.action)
read.adv.sontek.serial(file, from=1, to, by=1, tz=getOption("oce.tz"),
  type="default",
  latitude=NA, longitude=NA,
  start, deltat,
  debug=getOption("oce.debug"), monitor=TRUE, log.action)
read.adv.sontek.adr(file, from=1, to, by=1, tz=getOption("oce.tz"),
  header=TRUE, 
  latitude=NA, longitude=NA,
  type="",
  debug=getOption("oce.debug"), monitor=TRUE, log.action)
read.adv.sontek.text(basefile, from=1, to, by=1, tz=getOption("oce.tz"),
  coordinate.system="xyz",
  transformation.matrix,
  latitude=NA, longitude=NA,
  debug=getOption("oce.debug"), log.action)
}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to load.  It is also possble to supply a vector of filenames,
    for the case of data split into files by a data logger, and in that
    case, \code{header} must be \code{FALSE}, \code{start} must be a
    vector of times, and \code{deltat} must be provided.}
  \item{basefile}{character string naming the base of filenames to load
    (used only by \code{read.adv.sontek.text}). The actual filenames are
    constructed by appending \code{".hd1"} and \code{".ts1"} to the base
    name.}
  \item{from}{index number of the first profile to be read, or the time
    of that profile, as created with \code{\link{as.POSIXct}} (hint: use
    \code{tz="UTC"}).  This argument is ignored if
    \code{header==FALSE}. See \dQuote{Examples}.}
  \item{to}{indication of the last profile to read, in a format matching
    that of \code{from}.  This is ignored if \code{header==FALSE}.}
  \item{by}{an indication of the stride length to use while walking
    through the file.    This is ignored if \code{header==FALSE}.
    Otherwise, if this is an integer, then \code{by-1} profiles are
    skipped between each pair of profiles that is read. This may not make
    much sense, if the data are not equi-spaced in time.  If \code{by}
    is a string representing a time interval, in colon-separated format,
    then this interval is divided by the sampling interval, to get the
    stride length. \emph{BUG:} if the data are not equi-spaced, then odd
    results will occur.}
  \item{header}{a boolean indicating whether the file contains a 
    header at the start.  (This will not be the case for files
    that are created by data loggers that chop the raw data up into a
    series of sub-files, e.g. once per hour.)}
  \item{latitude}{optional signed number indicating the latitude in 
    degrees North.}
  \item{longitude}{optional signed number indicating the longitude in 
    degrees East.}
  \item{start}{the time of the first sample, typically created with
    \code{\link{as.POSIXct}}.  This is mandatory if \code{header} is
    \code{FALSE}.  This may be a vector of times, if \code{filename} is
    a vector of file names.}
  \item{deltat}{the time between samples. (This is mandatory if
    \code{header=FALSE}.)}
  \item{coordinate.system}{character string indicating coordinate system, one
    of \code{"beam"}, \code{"xyz"}, \code{"enu"} or \code{"other"}.}
  \item{transformation.matrix}{transformation matrix to use in
    converting beam coordinates to xyz coordinates.  This will over-ride
    the matrix in the file header, if there is one.  An example is
    \code{rbind(c(2.710,
    -1.409, -1.299), c(0.071, 2.372, -2.442), c(0.344, 0.344,
    0.344))}.}
  \item{type}{character string indicating type of file (ignored at present).}
  \item{tz}{character string indicating time zone to be assumed in the data.}
  \item{debug}{a flag that turns on debugging.  The value indicates the
    depth within the call stack to which debugging applies.  For
    example, \code{read.adv.nortek()} calls \code{read.header.nortek()},
    so that \code{read.adv.nortek(...,debug=2)} provides information
    about not just the main body of the data file, but also the details
    of the header.}
  \item{monitor}{boolean, set to \code{TRUE} to provide an indication
    of every data burst read.}
  \item{log.action}{if provided, the action item to be stored in the
    log.  This parameter is typically only provided for internal
    calls; the default that it provides is better for normal calls by
    a user.}
}

\details{Reads a binary-format ADV file.  This is straightforward for
  files with headers, since the headers contain all the information
  required for further processing.

  Files \emph{without} headers may be created in experiments in which a
  data logger was set up to monitor the serial data stream from an
  instrument.  The lack of header information places a burden on the
  user, who must supply such basic information as the times of
  observations, the instrument orientation, the instrument coordinate
  system, etc.  Example 3 below shows how to deal with such files.
  Three things should be noted.

  \enumerate{

    \item The use must choose the appropriate \code{read.adv} variant
    corresponding to the instrument in question.  (This is necessary
    because \code{\link{magic}}, which is used by the generic
    \code{\link{read.oce}} routine, cannot determine the type of
    instrument by examining a file that lacks a header.)

    \item The call to the \code{read} function must include a start time
    (\code{start}) and the number of seconds between data
    (\code{deltat}), again, because the instrument data stream may lack
    those things when the device is set to a serial mode.  Also, of
    course, it is necessary to set \code{header=FALSE} in the function
    call.

    \item Once the file has been read in, the user will be obliged to
    specify other information, for the object to be well-formed.  For
    example, the \code{read} function will have no way of knowing the
    instrument orientation, the coordinate system being used, the
    transformation matrix to go from \code{"beam"} to \code{"xyz"}
    coordinates, or the instrument heading, pitch, and roll, to go from
    \code{"xyz"} coordinates to \code{"enu"} coordinates.  Such things
    are illustrated in example 3 below.

  }

}

\section{Special considerations for NorTek files.}{

  \enumerate{

    \item The full file is first read into a buffer.

    \item Next, velocity chunks (at which velocities are stored, but not
    heading, etc.), are located within the buffer by using
    \code{matchBytes(buf, 0xa5, 0x10)}.  \emph{BUG:} the checksum
    provided in bytes 23 and 24 of these chunks should be examined to
    discard matches that occur randomly (with probability 1:65025) but
    this is not done in the present version of the
    \code{read.adv.nortek} function.

    \item System chunks (which hold heading, pitch, etc.) are found with
    \code{matchBytes(buf, 0xa5, 0x11, 0x0e)}.

    \item Times from the system chunks are mapped to velocity chunks by
    \emph{assuming} a constant sampling rate for each chunk.  If this
    assumption is incorrect, then the times in the \code{data$ts$time}
    vector will be incorrect, although the values in the
    \code{data$ts$pressure} vector will be correct.

    \item Unlike related functions, \code{read.adv.nortek} reads and
    decodes every system chunk, even if a \code{by} argument is
    specified.  This makes this function slow, because decoding times is
    a slow operation.  (About 60 percent of the execution time is spent
    calling \code{\link{ISOdatetime}}.)

  }
}

\section{Special considerations for Sontek files.}{


  The binary format is inferred from Appendix 2.2.3 of the Sontek ADV
  operation Manual, Firmware Version 4.0 (Oct 1997), with the following
  exceptions and notes.

  \enumerate{

    \item The documentation says sampling rate is in units of 0.1Hz, but
    a test file indicates that it is in 0.01 Hz.

    \item Bursts are recognized by byte sequences, as documented on page
    95 of [1].  In each case, a signalling byte is to be followed by a
    certain number of bytes, and so this code checks for two-byte
    sequences.  The are as follows:

    \itemize{
      \item \code{c(0x81,0x12)} for an ADV with no optional sensors
      installed.

      \item \code{c(0x83,0x18)} if a compass/tilt sensor is installed,
      but no temperature or pressor sensors.

      \item \code{c(0x85,0x16)} if temperature and/or pressure sensors
      are installed, but no compass/tilt sensor.

      \item \code{c(0x87,0x1c)} if a compass/tilt sensor is installed
      in addition to temperature and/or pressure sensors.

    }

    \strong{Bug:} only the second-last of these is handled in the present
    version of the package.
    
   }
 }

\value{An object of \code{\link[base]{class}} \code{"adv"}, which
  contains measurements made with an ADV device.  This is a list
  containing lists named \code{metadata}, \code{data}, and
  \code{processing.log}.  The \code{data} list contains lists \code{ts},
  potentially (for Nortek Vector units) \code{ts.slow}, and \code{ma}
  (matrices of velocity, etc).  These are as \code{adp} objects, except
  for the presence of \code{ts.slow}, for timeseries that vary on a slow
  timescale, e.g. on Nortek Vector units, \code{ts.slow} holds
  \code{heading}, \code{pitch}, \code{roll}, and \code{temperature},
  while \code{ts} holds only \code{pressure}.  }

\seealso{Objects of class \code{adv} may be plotted with
  \code{\link{plot.adv}} or summarized with \code{\link{summary.adv}}.
  Coordinate transformations are done with \code{\link{beam.to.xyz.adv}},
  \code{\link{xyz.to.enu.adv}}, and \code{\link{enu.to.other.adv}}, in
  that order.}

\examples{
\dontrun{
library(oce)
opar <- par(no.readonly = TRUE)
dir <- "/data/archive/sleiwex/2008/moorings/"

# Example 1
# Nortek binary file, daily at 1/2 hour decimation
d <- read.oce(
  "/data/archive/sleiwex/2008/moorings/m05/adv/nortek_1943/raw/adv_nortek_1943.vec",
  from=as.POSIXct("2008-06-26 00:00:00", tz="UTC"),
  to=as.POSIXct("2008-06-27 00:00:00", tz="UTC"),
  by="00:30:00")
plot(d, which=c(1:3,15))      # beams plus pressure

# Example 2
# Sontek adr file, with method to estimate turbulent dissipation
d <- read.adv.sontek.adr(
  "/data/archive/sleiwex/2008/moorings/m03/adv/sontek_b373h/raw/adv_sontek_b373h.adr",
  from=as.POSIXct("2008-07-01 15:30:00",tz="UTC"),
  to=as.POSIXct("2008-07-01 16:00:00",tz="UTC"))
w <- ts(d$data$ma$v[,3], deltat=1/d$metadata$sampling.rate) # z component
par(mfrow=c(2,2))
par(mgp=getOption("oce.mgp"))
par(mar=c(3,3,2,1))
plot(w)
plot(d$data$ts$time, sqrt(d$data$ma$v[,1]^2+d$data$ma$v[,2]^2),
    type='l', xlab="Time [s]", ylab="Horizontal velocity [m/s]")
U <- mean(sqrt(d$data$ma$v[,1]^2 + d$data$ma$v[,2]^2))
abline(h=U, col='red')
mtext(sprintf("U=\%.2fm/s", U), side=4, at=U)
s <- spectrum(w, spans=c(3,5,7), plot=FALSE)  # just guessing on spans
plot(log10(s$freq), log10(s$spec), type='l', xlab="f [Hz]", ylab="E", asp=1)
for (a in seq(-20, 10, 1))
    abline(a, -5/3, col='lightgray')     # slope if turbulent
k <- 2 * pi * s$freq / U
plot(log10(k), log10(k^(5/3)*s$spec), type='l', 
     xlab="k [radian/m]", ylab=expression(k^(5/3)*E), asp=1)
alpha <- 0.4                    # a 3D Kolmogorov coefficient
epsilon <- (median(k^(5/3)*s$spec)/alpha)^(3/2) # median might ignore low-freq
abline(h=log10(alpha * epsilon^(2/3)), col="red")
mtext(sprintf("\%.0e W/kg", epsilon), side=4, at=log10(alpha * epsilon^(2/3)))
grid()
par(opar)

# Example 3
# Sontek ADV dataset, chopped into one-hour files by a data logger.
library(oce)
table <- logger.toc("/data/archive/sleiwex/2008/moorings/m05/adv/sontek_202h/raw",
                    from=as.POSIXct("2008-07-01 00:00:00", tz="UTC"),
                    to=as.POSIXct("2008-07-01 2:00:00", tz="UTC"))
d <- read.adv.sontek.serial(file=table$filename, start=table$start.time, deltat=1/10)
d <- oce.edit(d, "coordinate.system", "xyz") # original coordinate
d <- oce.edit(d, "oce.coordinate", "xyz")    # present coordinate
# Note that the transformation matrix varies from instrument to instrument.
d <- oce.edit(d, "transformation.matrix", rbind(c(11033,-8503,-5238),
                                                c(347,-32767,9338),
                                                c(-1418, -1476, -1333)) / 4096)
d <- oce.edit(d, "orientation", "upward")
d <- oce.edit(d, "heading", 0)
d <- oce.edit(d, "pitch", 0)
d <- oce.edit(d, "roll", 0)
d <- oce.edit(d, "serial.number", "202h")
enu <- xyz.to.enu.adv(d)
plot(enu, which=c("u1", "u2", "u3", "temperature"))
}
}

\references{

1. SonTek/YSI ADVField/Hydra Acoustic Doppler Velocimeter (Field)
Technical Documentation (Sept 1, 2001).

}

\author{Dan Kelley}

\keyword{misc}
