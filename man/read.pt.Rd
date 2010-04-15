\name{read.pt}

\alias{read.pt}

\title{Read an TDR temperature-depth recorder data file}

\description{Read an pT temperature-depth recorder file, producing an object of type \code{pt}.}

\usage{read.pt(file, from="start", to="end", by=1, tz=getOption("oce.tz"), log.action, debug=getOption("oce.debug"))}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to load.}
  \item{from}{indication of the first datum to read.  This can be the
    word \code{"start"}, for the first datum in the file, or positive
    integer giving the sequence number (1 thus being equivalent to
    \code{"start"}), or the POSIX time of the first datum, or a
    character string that can be converted to a POSIX time.  (For POSIX
    times, be careful about the \code{tz} argument.)}
  \item{to}{an indication of the last datum to be read.  The string
    \code{"end"} means to read to the end of the file; otherwise, the
    indication is of the same format as for \code{from}.}
  \item{by}{an indication of the stride length to use while walking
    through the file.  If this is an integer, then \code{by-1}
    profiles are skipped between each pair of profiles that is read.  If
    this is a string representing a time interval, in colon-separated
    format (HH:MM:SS or MM:SS), then this interval is divided by the
    sampling interval, to get the stride length.}
  \item{tz}{time zone.  The default value, \code{oce.tz}, is set to \code{UTC} 
    at setup.}
  \item{log.action}{if provided, the action item to be stored in the
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
      \item{\code{logging.start}}{start time for logging, inferred from
	the header.  Caution: this is often not the first time in the
	data, because the data may have been subsetted.}
      \item{\code{sample.period}}{seconds between samples, inferred from
	the header.  Caution: this is often not the sampling period in the
	data, because the data may have been decimated.}
    }
  }
  \item{processing.log}{a processing log, in the standard \code{oce} format.}
}

\seealso{The generic function \code{\link{read.oce}} provides an
  alternative to this.  Objects of type \code{pt} can be plotted
  with \code{\link{plot.pt}}, and summarized with
  \code{\link{summary.pt}}, both of which are generic functions.
  In-air samples (collected before and after deployment) may be removed
  in a crude way with \code{\link{pt.trim}}, but the best scheme is
  to use \code{\link{subset.oce}}, based on a temporal window (see
  Examples).  Removal of the atmospheric component of pressure is left
  up to the user; in many cases, it makes sense to treat this as a
  constant (see Examples).}

\examples{
\dontrun{
pt <- read.pt("sl08_011855.dat", by=600)
save(pt, file="oce/data/pt.rda")
}
}

\author{Dan Kelley}

\keyword{misc}
