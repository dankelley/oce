% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{read.rsk}

\alias{read.rsk}

\title{Read an RBR rsk or Ruskin export file}

\description{Read an RBR rsk or txt file, e.g. as produced by an RBR logger, producing an
  object of class \code{rsk}.}

\usage{read.rsk(file, from=1, to, by=1, type,
            tz=getOption("oceTz", default="UTC"), patm=FALSE,
            processingLog, debug=getOption("oceDebug"))}

\arguments{
  \item{file}{a connection or a character string giving the name of the file to
    load. Note that \code{file} must be a character string, because connections
    are not used in that case, which is instead handled with database calls.}
  \item{from}{indication of the first datum to read.  This can a
    positive integer to indicate sequence number, the POSIX time of the
    first datum, or a character string that can be converted to a POSIX
    time.  (For POSIX times, be careful about the \code{tz} argument.)}
  \item{to}{an indication of the last datum to be read, in the same
    format as \code{from}.  If \code{to} is missing, data will be read
    to the end of the file.}
  \item{by}{an indication of the stride length to use while walking
    through the file.  If this is an integer, then \code{by-1}
    samples are skipped between each pair of samples that is read.  If
    this is a string representing a time interval, in colon-separated
    format (HH:MM:SS or MM:SS), then this interval is divided by the
    sampling interval, to get the stride length.}
  \item{type}{optional file type, presently can be \code{rsk} or
  \code{txt} (for a text export of an RBR rsk or hex file). If
    this argument is not provided, an attempt will be made to infer the type
    from the file name and contents.}
  \item{tz}{time zone.  The value \code{oceTz} is set at package setup.}

  \item{patm}{controls the handling of atmospheric pressure, an important issue
    for RBR instruments that record absolute pressure; see \dQuote{Details}.}

  \item{processingLog}{if provided, the action item to be stored in the log.
    (Typically only provided for internal calls; the default that it provides is
    better for normal calls by a user.)}

  \item{debug}{a flag that can be set to \code{TRUE} to turn on debugging.}
}

\details{This can read files produced by several RBR instruments.  At the
  moment, five styles are understood: (1) text file produced as an
  export of an RBR \code{hex} or \code{rsk} file; (2) text file with columns for temperature
  and pressure (with sampling times indicated in the header); (3) text file with
  four columns, in which the date the time of day are given in the first two
  columns, followed by the temperature, and pressure; (4) text file with five
  columns, in which depth in the water column is given after the pressure; (5)
  an SQLite-based database format. The first four options are provided mainly
  for historical reasons, since RBR instruments at the date of writing commonly
  use the SQLite format, though the first option is common for all
  instruments that produce a \code{hex} file that can be read using
  Ruskin.

  Options 2-4 are mostly obsolete, and will be removed from future versions.

  \emph{A note on conductivity.} RBR devices record conductivity in mS/cm, and
  that it is this value that is stored in the object returned by
  \code{read.rsk}. This can be converted to conductivity ratio (which is what
  many other instruments report) by dividing by 42.914, which will be necessary
  in any seawater-related function that takes conductivity ratio as an argument
  (see \dQuote{Examples}).
    
  \emph{A note on pressure.} RBR devices tend to record absolute pressure (i.e.
  sea pressure plus atmospheric pressure), unlike most oceanographic instruments
  that record sea pressure (or an estimate thereof).  The handling of pressure
  is controlled with the \code{patm} argument, for which there are three
  possibilities.  (1) If \code{patm} is \code{FALSE} (the default), then
  pressure read from the data file is stored in the \code{data} slot of return
  value, and the \code{metadata} item \code{pressureType} is set to the string
  \code{"absolute"}.  (2) If \code{patm} is \code{TRUE}, then an estimate of
  atmospheric pressure is subtracted from the raw data. For data files in the
  SQLite format (i.e.  \code{*.rsk} files), this estimate will be the value read
  from the file, or the ``standard atmosphere'' value 10.1325 dbar, if the file
  lacks this information.  (3) If \code{patm} is a numerical value (or list of
  values, one for each sampling time), then \code{patm} is subtracted from the
  raw data.  In cases 2 and 3, an additional column named
  \code{pressureOriginal} is added to the \code{data} slot to store the value
  contained in the data file, and \code{pressureType} is set to a string
  starting with \code{"sea"}.  See \code{\link{as.ctd}} for details of how this
  setup facilitates the conversion of \code{\link{rsk-class}} objects to
  \code{\link{ctd-class}} objects.

}

\value{An object of \code{\link{rsk-class}}.}

\examples{
library(oce)
## A CTD-style rsk
if (requireNamespace("R.utils")) {
  file <- system.file("extdata", "sample.rsk.gz", package="oce")
  tmp <- tempfile(fileext=".rsk")
  R.utils::decompressFile(file, destname=tmp, ext="gz", FUN=gzfile, remove=FALSE)
  rsk <- read.rsk(tmp)
  ctd <- as.ctd(rsk)
  plot(ctdTrim(ctd))
}
}

\seealso{The documentation for \code{\link{rsk-class}} explains the
    structure of \code{rsk} objects, and also outlines other functions
    dealing with them. See \code{\link{as.ctd}} for the method of converting to
    a CTD object.}

\author{Dan Kelley and Clark Richards}

\keyword{misc}
