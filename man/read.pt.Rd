\name{read.pt}

\alias{read.pt}

\title{Read an TDR temperature-depth recorder data file}

\description{Read an pt temperature-depth recorder file, producing an object of type \code{pt}.}

\usage{
read.pt(file, from=1, to, by=1, tz=getOption("oceTz"), 
        processingLog, debug=getOption("oceDebug"))}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to load.}
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
  \item{tz}{time zone.  The default value, \code{oceTz}, is set to \code{UTC} 
    at setup.}
  \item{processingLog}{if provided, the action item to be stored in the
    log.  (Typically only provided for internal calls; the default that
    it provides is better for normal calls by a user.)}
  \item{debug}{a flag that can be set to \code{TRUE} to turn on debugging.}
}

\details{Read an TDR (temperature-depth recorder) file.  At the moment,
  three styles are understood: (1) a two-column style, containing
  temperature and pressure (in which case time is inferred from
  information in the header); (2) a four-column style, in which the date
  the time of day are given in the first two columns, followed by the
  temperature, and pressure; and (3) a five-column style, in which depth
  in the water column is given after the pressure.}

\value{An object of \code{\link[base]{class}} \code{"pt"}, which is a
  list with elements detailed below.
  \item{data}{a data table containing the \code{time},
    \code{temperature}, and \code{pressure} data.}
  \item{metadata}{a list containing the following items
    \describe{
      \item{\code{header}}{the header itself, as read from the input file.}
      \item{\code{serial.number}}{serial number of instrument, inferred
	from first line of the header.}
      \item{\code{loggingStart}}{start time for logging, inferred from
	the header.  Caution: this is often not the first time in the
	data, because the data may have been subsetted.}
      \item{\code{sample.period}}{seconds between samples, inferred from
	the header.  Caution: this is often not the sampling period in the
	data, because the data may have been decimated.}
    }
  }
  \item{processingLog}{a processingLog of processing, in the standard \code{oce} format.}
}

\section{Implementation notes}{The end time for measurement
  (\code{metadata$measurement.start}) is inferred from the \code{Logger
  time} field in the header, not from \code{Logging end}.  The datasets
  available to the author suggest this is the proper scheme when the
  recorders are turned off manually before the end time that was
  programmed in.  In other cases, the assumption may or may not be
  correct.  Still, the end time for subsampling
  (\code{metdata$subsample.end}) should be correct.}


\seealso{The generic function \code{\link{read.oce}} provides an
  alternative to this.  Objects of type \code{pt} can be plotted with
  \code{\link{plot.pt}}, and summarized with \code{\link{summary.pt}},
  both of which are generic functions.  In-air samples (collected before
  and after deployment) may be removed in a crude way with
  \code{\link{ptTrim}}, but the best scheme is to use
  \code{\link{subset.oce}}, based on a temporal window.  Removal of the
  atmospheric component of pressure is best done by the user, using
  measured records of atmospheric pressure.  However, if such data are
  unavailable, and if the p-T data set contains pre- and post-deployment
  data, \code{\link{ptPatm}} may provide a useful estimate of
  atmospheric pressure.}

\examples{
\dontrun{
pt <- read.pt("sl08_011855.dat", by=600)
save(pt, file="oce/data/pt.rda")
}
}

\author{Dan Kelley}

\keyword{misc}
