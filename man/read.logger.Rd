\name{read.logger}

\alias{read.logger}

\title{Read a logger file}

\description{Read a logger file, e.g. a temperature-depth recorder, producing
    an object of type \code{logger}.}

\usage{read.logger(file, from=1, to, by=1, type, tz=getOption("oceTz"),
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
  \item{tz}{time zone.  The default value, \code{oceTz}, is set to \code{UTC} 
    at setup.}
  \item{processingLog}{if provided, the action item to be stored in the
    log.  (Typically only provided for internal calls; the default that
    it provides is better for normal calls by a user.)}
  \item{debug}{a flag that can be set to \code{TRUE} to turn on debugging.}
}

\details{Read a logger file produced by one of several instruments produced by
    RBR.  At the moment, four styles are understood: (1) text file with columns
    for temperature and pressure (with sampling times indicated in the header);
    (2) text file with four columns, in which the date the time of day are
    given in the first two columns, followed by the temperature, and pressure;
    (3) text file with five columns, in which depth in the water column is
    given after the pressure; (4) an SQLite-based database format.}

\value{If the data contain conductivity, temperature and pressure, then
    \code{read.logger} coerces its results into an object of
    \code{\link[base]{class}} \code{"ctd"} (see \code{\link{ctd-class}}), after
    subtracting atmospheric pressure (which is stored in RBR files).
    Otherwise, return an object of \code{\link[base]{class}} \code{"logger"}
    (see \code{\link{logger-class}}).  }

\seealso{The documentation for \code{\link{logger-class}} explains the
    structure of \code{logger} objects, and also outlines the other functions
    dealing with them.}

\author{Dan Kelley}

\keyword{misc}
