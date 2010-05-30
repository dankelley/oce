\name{read.adv}
\alias{read.adv}
\alias{read.adv.nortek}
\alias{read.adv.sontek}
\alias{read.adv.sontek.adr}
\alias{read.adv.sontek.text}

\title{Read an ADV data file}

\description{Read an ADV data file, producing an object of type \code{adv}.}

\usage{
read.adv(file, from=1, to, by=1, tz=getOption("oce.tz"),
        type=c("nortek", "sontek", "sontek.adr", "sontek.text"),
        header=TRUE,
        start, deltat,
        debug=getOption("oce.debug"), monitor=TRUE, log.action)
read.adv.nortek(file, from=1, to, by=1, tz=getOption("oce.tz"), 
                type="vector",
                header=TRUE,
                debug=getOption("oce.debug"), monitor=TRUE, log.action)
read.adv.sontek(file, from=1, to, by=1, tz=getOption("oce.tz"),
                type="default",
                header=TRUE,
                start, deltat,
                debug=getOption("oce.debug"), monitor=TRUE, log.action)
read.adv.sontek.adr(file, from=1, to, by=1, tz=getOption("oce.tz"),
                    header=TRUE, type="",
                    debug=getOption("oce.debug"), monitor=TRUE, log.action)
read.adv.sontek.text(basefile, from=1, to, by=1, tz=getOption("oce.tz"),
                     coordinate.system="xyz",
                     transformation.matrix,
                     debug=getOption("oce.debug"), log.action)
}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to load.}
  \item{basefile}{character string giving the base name of files to load
    (used only by read.adv.sontek.text). The actual filenames are constructed
    by appending \code{".hd1"} and \code{".ts1"} to the base name.}
  \item{from}{index number of first profile to be read, or the
    (POSIXt) time of that first profile.  This is ignored if
    \code{header=FALSE}. See \dQuote{Examples}, and make careful note of
    the use of the \code{tz} argument.}
  \item{to}{indication of the last profiel to read, in a format matching
    that of \code{from}.  This is ignored if \code{header=FALSE}.}
  \item{by}{an indication of the stride length to use while walking
    through the file.    This is ignored if \code{header=FALSE}.
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
  \item{start}{a POSIXct time corresponding to the first sample. 
    (This is mandatory if \code{header} is \code{FALSE}.)}
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

\details{Reads a binary-format ADV file.  This is relatively
  straightforward if there is a header.  If not, i.e. if
  \code{header} is \code{FALSE}, then \code{read.adv} is forced
  to make guesses about important things, and the wise user will
  check on the elements of \code{metadata} in the return value,
  rectifying any that are wrong, and adding any that are missing.  At a
  bare minimum, cases with no header require the user to give the
  \code{start} time and the time \code{deltat} between samples, since,
  without these, no time base can be set up.

  \emph{Notes on NorTek files.}

  \enumerate{

    \item The full file is first read into a buffer. Velocity chunks are
    located within the buffer by using \preformatted{vc <- match.bytes(buf, 0xa5, 0x10)} which yields a vector of indices
    within \code{buf}, at which the velocity chunks start.  \emph{BUG:}
    the checksum provided in bytes 23 and 24 of these chunks should be
    examined to discard matches that occur randomly (with probability
    1:65025) but this is not done in the present version of oce. (FIXME:
    check this ... I think the code reads the checksum now.)

    \item System chunks are recognized by using
    \preformatted{sc <- match.bytes(buf, 0xa5, 0x11, 0x0e)}
    and it is these chunks that contain the times of observation, along
    with the orientation of the device.

    \item Times from the system chunks are coordinated with velocities
    in the velocity chunks by ... \emph{BUG:} fill in the method.

  }

  \emph{Notes on SonTek ADR files.}
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
  contains measurements made with an ADV device.  For information on
  data stored in the object, see \dQuote{Details}.}

\seealso{Objects of class \code{adv} may be plotted with
  \code{\link{plot.adv}} or summarized with \code{\link{summary.adv}}.
  Coordinate transformations are done with \code{\link{adv.beam2xyz}},
  \code{\link{adv.xyz2enu}}, and \code{\link{adv.enu2other}}, in
  that order.}

\examples{
\dontrun{
library(oce)

dir <- "/data/archive/sleiwex/2008/moorings/"

## Nortek binary file, daily at 1/2 hour decimation
f <- paste(dir, "m05/adv/nortek_1943/raw/adv_nortek_1943.vec", sep="")
from <- as.POSIXct("2008-06-26 00:00:00", tz="UTC")
to <- as.POSIXct("2008-06-27 00:00:00", tz="UTC")
by <- "00:30:00"
d <- read.adv.nortek(f, from=from, to=to, by=by)
plot(d, which=c(1:3,15))      # beams plus pressure

## Sontek file, chopped into hourly files with a data logger
f <- paste(dir, "m05/adv/sontek_202h/raw/adv_sontek_202h.183.001", sep="")
d <- read.adv.sontek(f, header=FALSE, start="2008-07-01 00:00:00", deltat=0.1)
d$metadata$serial.number <- "B202H"
d$metadata$orientation <- "down"
summary(d)
plot(d, which=c(1:3,14))      # skip pressure (not measured)

## Sontek adr file, with method to estimate turbulent dissipation
f <- paste(dir, "m03/adv/sontek_b373h/raw/adv_sontek_b373h.adr", sep="")
d <- read.adv.sontek.adr(f, from=as.POSIXct("2008-07-01 15:30:00",tz="UTC"),
                            to=as.POSIXct("2008-07-01 16:00:00",tz="UTC"))
d$data$ma$v[,2] <- -d$data$ma$v[,2] # account for deployment setup
d$data$ma$v[,3] <- -d$data$ma$v[,3] # account for deployment setup
w <- ts(adv$data$ma$v[,3], deltat=1/adv$metadata$sampling.rate)
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

}
}

\references{

1. SonTek/YSI ADVField/Hydra Acoustic Doppler Velocimeter (Field)
Technical Documentation (Sept 1, 2001).

}

\author{Dan Kelley}

\keyword{misc}
