\name{read.adv}
\alias{read.adv}
\alias{read.adv.nortek}
\alias{read.adv.sontek.serial}
\alias{read.adv.sontek.adr}
\alias{read.adv.sontek.text}

\title{Read an ADV data file}

\description{Read an ADV data file, producing an object of type \code{adv}.}

\usage{
read.adv(file, from=1, to, by=1, tz=getOption("oceTz"),
  type=c("nortek", "sontek", "sontek.adr", "sontek.text"),
  header=TRUE,
  latitude=NA, longitude=NA,
  start, deltat,
  debug=getOption("oceDebug"), monitor=FALSE, processingLog)
read.adv.nortek(file, from=1, to, by=1, tz=getOption("oceTz"), 
  type="vector",
  header=TRUE,
  latitude=NA, longitude=NA,
  debug=getOption("oceDebug"), monitor=FALSE, processingLog)
read.adv.sontek.serial(file, from=1, to, by=1, tz=getOption("oceTz"),
  type="default",
  latitude=NA, longitude=NA,
  start, deltat,
  debug=getOption("oceDebug"), monitor=FALSE, processingLog)
read.adv.sontek.adr(file, from=1, to, by=1, tz=getOption("oceTz"),
  header=TRUE, 
  latitude=NA, longitude=NA,
  type="",
  debug=getOption("oceDebug"), monitor=FALSE, processingLog)
read.adv.sontek.text(basefile, from=1, to, by=1, tz=getOption("oceTz"),
  coordinateSystem="xyz",
  transformationMatrix,
  latitude=NA, longitude=NA,
  debug=getOption("oceDebug"), monitor=FALSE, processingLog)
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
  \item{coordinateSystem}{character string indicating coordinate system, one
    of \code{"beam"}, \code{"xyz"}, \code{"enu"} or \code{"other"}.}
  \item{transformationMatrix}{transformation matrix to use in
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
  \item{processingLog}{if provided, the action item to be stored in the
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

    The data format is inferred from the System Integrator Guide [1].  This
    document lacks clarity in spots, and so \code{read.adv.nortek} contains
    some assumptions that should be noted, in case they fail for datasets other
    than the ones tested to date.

    A prominent example is the specification of the sampling rate, stored in
    \code{metadata$sampingRate} in the return value.  Repeated examination of
    the System Integrator Guide [1] failed to indicate where this value is
    stored in the various headers contained in Vector datasets.  In an early
    version of \code{read.adv.nortek}, the sampling rate was inferred from the
    mean time between data; this may be a reasonable approximation for data
    sampled continuously, but it fails completely for burst-mode data.
    
    After some experimentation with a few data files, \code{read.adv.nortek}
    was set up to calculate \code{metadata$samplingRate} as
    \code{512/AvgInterval} where \code{AvgInterval} is a part of the ``User
    Configuration'' header (see page 30 of [1], where the explanation is
    ``average interval in seconds'').  This formula was developed through trial
    and error, but it was confirmed later on the Nortek discussion group, and
    it should appear in upcoming versions of [1].
    
    Another important issue is the determination of whether an instrument had
    recorded in continuous mode or burst mode.  One might infer that
    \code{TimCtrlReg} in the ``User Configuration'' header (p30 of [1])
    determines this, in bits 1 and 2.  However, this cannot be the case, based
    on examination of those bits in files of the two types.  For this reason,
    \code{read.adv.nortek} is left to infer the mode by reverse engineering of
    data files of known configuration.  The kludge in this instance is to
    examine the ``\code{NRecords}'' item of the ``Vector Velocity Data''
    header.    It appears that this is 0 for data collected continuously, and
    non-zero for data collected in bursts.  
    
    The reader may join the writer in hoping that future versions of
    \code{read.adv.nortek} can be based less on guesswork, more on
    documentation.

}

\section{Special considerations for Sontek files.}{


  The binary format is inferred from Appendix 2.2.3 of the Sontek ADV operation
  Manual [3], with the following exceptions and notes.

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
      but no temperature or pressure sensors.

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
  \code{processingLog}.  
  
  The \code{metadata} contains information as given in the following table.
  The ``Nortek name'' is the name used in the Nortek System Integrator Guide
  [reference 1] and the ``Sontek name'' is the name used in the relevant Sontek
  documentation.  Numbers in parentheses indicate page numbers in the relevant
  documents.

  \tabular{llll}{
      \strong{\code{metadata} name}\tab \strong{Nortek name} \tab \strong{Sontek name}\tab\strong{Meaning}\cr
      \code{manufacturer}\tab - \tab - \tab Either \code{"nortek"} or \code{"sontek"}\cr
      \code{instrumentType}\tab - \tab - \tab Either \code{"vector"} or \code{"adv"}\cr
      \code{filename}\tab - \tab - \tab Name of data file(s)\cr
      \code{latitude}\tab - \tab - \tab Latitude of mooring (if applicable)\cr
      \code{longitude}\tab - \tab - \tab Longitude of mooring (if applicable)\cr
      \code{numberOfSamples}\tab - \tab - \tab Number of data samples in file\cr
      \code{numberOfBeams}\tab NBeams (18) \tab - \tab Number of beams (always 3)\cr
      \code{numberOfBeamSequencesPerBurst}\tab NPings \tab - \tab number of beam sequences per burst\cr
      \code{measurementInterval}\tab MeasInterval (31) \tab - \tab \cr
  }

  The \code{data} list contains items with names corresponding to 
  \code{adp} objects, with an exception for Nortek data.  Nortek instruments
  report some things at a time interval that is longer than the velocity sampling, and these
  are stored in \code{data} as \code{timeSlow}, \code{headingSlow},
  \code{pitchSlow}, \code{rollSlow}, and \code{temperatureSlow}.

  The \code{processingLog} is in the standard format.
}

\seealso{Objects of class \code{adv} may be plotted with
  \code{\link{plot.adv}} or summarized with \code{\link{summary.adv}}.
  Coordinate transformations are done with \code{\link{beamToXyzAdv}},
  \code{\link{xyzToEnuAdv}}, and \code{\link{enuToOtherAdv}}, in
  that order.}

\examples{
\dontrun{
library(oce)
# Nortek binary file, daily at 1/2 hour decimation
d <- read.oce(
  "/data/archive/sleiwex/2008/moorings/m05/adv/nortek_1943/raw/adv_nortek_1943.vec",
  from=as.POSIXct("2008-06-26 00:00:00", tz="UTC"),
  to=as.POSIXct("2008-06-27 00:00:00", tz="UTC"),
  by="00:30:00")
plot(d, which=c(1:3,15))
}
}

\references{

1. Nortek AS.  System Integrator Guide (paradopp family of products). June
2008.  (Doc No: PSI00-0101-0608)

2. SonTek/YSI ADVField/Hydra Acoustic Doppler Velocimeter (Field)
Technical Documentation (Sept 1, 2001).

3. Appendix 2.2.3 of the Sontek ADV operation Manual, Firmware Version 4.0 (Oct
1997).

4. Nortek Knowledge Center \url{http://www.nortekusa.com/en/knowledge-center}
}

\author{Dan Kelley}

\keyword{misc}
