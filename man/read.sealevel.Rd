\name{read.sealevel}
\alias{read.sealevel}
\title{Read a sea-level data file}
\description{
  Read a data file holding sea level data.
  BUG: the time vector assumes GMT, regardless of the GMT.offset value.
}
\usage{
read.sealevel(file, debug=FALSE)
}
\arguments{
  \item{file}{A connection or a character string giving the name of the
    file to load.}
  \item{debug}{Set to \code{TRUE} to get debugging information during processing.}
}

\details{This function starts by scanning the first line of the file, from which it determines
whether the file is in one of two known formats: the tabular format used at the Hawaii archive
centre, or the comma-separated-value format used by the Marine Environmental Data Service
(indicated by the existence of the string \code{Station_Name} on the first line
of the file). If the
file is in neither of these known formats, the user might wish to scan the data by some other
means, and then to use \code{\link{as.sealevel}} to create the \code{sealevel} object.}

\value{
  A \code{sealevel} object, which is a \code{\link{list}} containing
  \item{data}{A \code{\link{data.frame}} containing
    \item{\code{t}}{time}
    \item{\code{eta}}{sea level [m]}
  }
  \item{metadata}{A list containing
    \item{\code{header}}{the header line (helpful if detail extraction failed.)}
    \item{\code{year}}{year in which the observations were made.}
    \item{\code{station.number}}{identifier for the station.}
    \item{\code{station.version}}{see online docs at site mentioned in References.}
    \item{\code{station.name}}{name of station}
    \item{\code{region}}{a region code.}
    \item{\code{latitude}}{latitude, decimal degrees, positive north of equator.}
    \item{\code{longitude}}{longitude, decimal degrees, positive east of Greenwich.}
    \item{\code{GMT.offset}}{offset from GMT time.}
    \item{\code{decimation.method}}{see online docs at sites mentioned in References.}
    \item{\code{reference.offset}}{a reference offset; see online docs at sites mentioned in References.}
    \item{\code{reference.code}}{a reference code; see online docs at site mentioned in References.}
    \item{\code{units}}{unit of observations in the original file (normally "MM")}
    \item{\code{n}}{number of observations.}
  }
  \item{processing.log}{A processing log, in the standard \code{oce} format.}
}

\seealso{
The generic function \code{\link{read.oce}} provides an alternative to this.
A \code{sealevel} object may be summarized with \code{\link{summary.sealevel}} and plotted with
\code{\link{plot.sealevel}}.

A \code{sealevel} object can also be constructed with \code{\link{as.sealevel}}.
}
\examples{
\dontrun{
library(oce)
# San Francisco data
# ftp://ilikai.soest.hawaii.edu/rqds/pacific/hourly/h551a.zip
h <- read.sealevel("h551a/h551a01.dat")
summary(h)
plot(h)
}
}

\references{The Hawaii archive site at \url{http://ilikai.soest.hawaii.edu/uhslc/datai.html}
provides a graphical interface for downloading sealevel data; the format is described at
\url{ftp://ilikai.soest.hawaii.edu/rqds/hourly.fmt}. The MEDS data are at
\url{http://www.meds-sdmm.dfo-mpo.gc.ca/meds/Databases/TWL/TWL_inventory_e.htm}.}

\author{Dan Kelley}

\keyword{misc}
