\name{read.tdr}

\alias{read.tdr}

\title{Read an TDR temperature-depth recorder data file}

\description{Read an TDR temperature-depth recorder file, producing an object of type \code{tdr}.}

\usage{read.tdr(file, debug=FALSE, log.action)}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to load.}
  \item{debug}{a flag that can be set to \code{TRUE} to turn on debugging.}
  \item{log.action}{if provided, the action item to be stored in the
    log.  (Typically only provided for internal calls; the default that
    it provides is better for normal calls by a user.)}
}

\details{Read an TDR (temperature-depth recorder) file.}

\value{An object of \code{\link[base]{class}} \code{"tdr"}, which is a
  list with elements detailed below.
  \item{data}{a data table containing the (t, temperature, pressure)
    data.}
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
  alternative to this.  Objects of type \code{tdr} can be plotted
  with \code{\link{plot.tdr}}, and summarized with
  \code{\link{summary.tdr}}, both of which are generic functions.
  In-air samples (collected before and after deployment) may be removed
  in a crude way with \code{\link{tdr.trim}}, but the best scheme is
  to use \code{\link{subset.oce}}, based on a temporal window (see
  Examples).  Removal of the atmospheric component of pressure is left
  up to the user; in many cases, it makes sense to treat this as a
  constant (see Examples).}

\examples{
library(oce)
data(tdr)
# trim automatically
tdr.trim.auto <- tdr.trim(tdr)
# trim manually
plot(tdr, which=2)
# try a few times near start of record (15:00:00 seems good)
abline(v=as.POSIXct("2008-06-25 00:00:00"),col="red")
abline(v=as.POSIXct("2008-06-26 00:00:00"),col="red")
abline(v=as.POSIXct("2008-06-25 15:00:00"),col="red")
# try a few times near end of record (15:00:00 seems ok)
abline(v=as.POSIXct("2008-07-04 00:00:00"),col="blue")
abline(v=as.POSIXct("2008-07-05 10:00:00"),col="blue")
abline(v=as.POSIXct("2008-07-04 12:00:00"),col="blue")
tdr.trim.manual <- subset(tdr, as.POSIXct("2008-06-25 15:00:00") <= t &
t <= as.POSIXct("2008-07-04 12:00:00"))
plot(tdr.trim.manual)
}

\author{Dan Kelley}

\keyword{misc}
