\name{read.logger}

\alias{read.logger}

\title{Read a logger file}

\description{Read a logger file, e.g. a temperature-depth recorder, producing
    an object of class \code{logger}.} 

\usage{read.logger(file, from=1, to, by=1, type,
            tz=getOption("oceTz", default="UTC"), patm=FALSE,
            processingLog, debug=getOption("oceDebug"))}

\arguments{
  \item{file}{a connection or a character string giving the name of the file to
      load. (If it is an RSK logger, \code{file} must be a character string,
      because connections are not used in that case, which is instead handled
      with database calls.}
  \item{from}{indication of the first datum to read.  This can a
    positive integer to indicate sequence number, the POSIX time of the
    first datum, or a character string that can be converted to a POSIX
    time.  (For POSIX times, be careful about the \code{tz} argument.)}
  \item{to}{an indication of the last datum to be read, in the same
    format as \code{from}.  If \code{to} is missing, data will be read
    to the end of the file.}
  \item{by}{an indication of the stride length to use while walking
    through the file.  If this is an integer, then \code{by-1}
    profiles are skipped between each pair of profiles that is read.  If
    this is a string representing a time interval, in colon-separated
    format (HH:MM:SS or MM:SS), then this interval is divided by the
    sampling interval, to get the stride length.}
  \item{type}{optional file type, presently only permitted to be \code{rsk}. If
      this argument is not provided, an attempt will be made to infer the type
      from the file name and contents.}
  \item{tz}{time zone.  The value \code{oceTz} is set at package setup.}

  \item{patm}{controls the handling of atmospheric pressure, an important issue
      for RBR instruments that record total pressure, i.e. the sum of
      atmospheric pressure and sea pressure. There are three alternatives.  (1)
      If \code{patm} is \code{FALSE} (the default), then total pressures are
      put in the return value. This means that profile plots will start at
      what appears to be approximately 10 metres depth. It also leads to errors
      in the calculation of pressure-dependent seawater properties.  However,
      note that if salinity is calculated by \code{read.logger}, an estimate of
      atmospheric pressure is first subtracted. The estimate is
      \code{atmosphericPressure} in the data file or 10.1325 dbar if the file
      lacks that value. (2) If \code{patm} is \code{TRUE}, then the
      above-stated estimate of atmospheric pressure is subtracted from the raw
      data.  (3) If \code{patm} is a numerical value, then it will be
      subtracted from the raw data.}

  \item{processingLog}{if provided, the action item to be stored in the log.
      (Typically only provided for internal calls; the default that it provides
      is better for normal calls by a user.)}

  \item{debug}{a flag that can be set to \code{TRUE} to turn on debugging.}
}

\details{Read a logger file produced by one of several instruments produced by
    RBR.  At the moment, four styles are understood: (1) text file with columns
    for temperature and pressure (with sampling times indicated in the header);
    (2) text file with four columns, in which the date the time of day are
    given in the first two columns, followed by the temperature, and pressure;
    (3) text file with five columns, in which depth in the water column is
    given after the pressure; (4) an SQLite-based database format.

    It should be noted that RBR loggers record conductivity in mS/cm, and that
    it is this value that is stored in the object returned by
    \code{read.logger}. This can be converted to conductivity ratio (which is
    what many other instruments report) by dividing by 42.914, which will be
    necessary in any seawater-related function that takes conductivity ratio as
    an argument (see \dQuote{Examples}).
    
    Another particular aspect of RBR loggers is that they tend to record total
    pressure (i.e. sea pressure plus atmospheric pressure), many applications
    will involve an alteration of the pressure. This can be done with the
    \code{patm} argument, of by subtraction of a constant (or vector) after
    creating the object.

}

\value{An object of \code{\link{logger-class}}.}

\examples{
\dontrun{
library(oce)
## A CTD-style logger
logger <- read.logger("cast4.rsk")
patm <- logger[["pressure"]][1]
ctd <- as.ctd(logger, pressureAtmospheric=patm)
plot(ctdTrim(ctd))
}
}

\seealso{The documentation for \code{\link{logger-class}} explains the
    structure of \code{logger} objects, and also outlines other functions
    dealing with them. See \code{\link{as.ctd}} for the method of converting to
    a CTD object.}

\author{Dan Kelley}

\keyword{misc}
