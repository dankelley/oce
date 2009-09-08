\name{read.pt}

\alias{read.pt}

\title{Read an TDR temperature-depth recorder data file}

\description{Read an pT temperature-depth recorder file, producing an object of type \code{pt}.}

\usage{read.pt(file, tz=getOption("oce.tz"), log.action, debug=getOption("oce.debug"))}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to load.}
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
library(oce)
data(pt)
# trim automatically
pt.trim.auto <- pt.trim(pt)
# trim manually
plot(pt, which=2)
# try a few times near start of record (15:00:00 seems good)
abline(v=as.POSIXct("2008-06-25 00:00:00"),col="red")
abline(v=as.POSIXct("2008-06-26 00:00:00"),col="red")
abline(v=as.POSIXct("2008-06-25 15:00:00"),col="red")
# try a few times near end of record (15:00:00 seems ok)
abline(v=as.POSIXct("2008-07-04 00:00:00"),col="blue")
abline(v=as.POSIXct("2008-07-05 10:00:00"),col="blue")
abline(v=as.POSIXct("2008-07-04 12:00:00"),col="blue")
pt.trim.manual <- subset(pt, as.POSIXct("2008-06-25 15:00:00") <= time &
                          time <= as.POSIXct("2008-07-04 12:00:00"))
plot(pt.trim.manual)
}

\author{Dan Kelley}

\keyword{misc}
