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
centre, or the comma-separated-value format used by the Marine Environmental Data Service. If the
file is in neither of these known formats, the user might wish to scan the data by some other
means, and then to use \code{\link{as.sealevel}} to create the \code{sealevel} object.}

\value{
  A \code{sealevel} object containing 
  \item{header}{The header line (helpful if detail extraction failed)}
  \item{station.number}{identifier for the station.}
  \item{station.version}{see online docs at site mentioned in References.}
  \item{station.name}{name of station}
  \item{region}{a region code.}
  \item{year}{year in which the observations were made.}
  \item{latitude}{latitude, decimal degrees, positive north of equator.}
  \item{longitude}{longitude, decimal degrees, positive east of Greenwich.}
  \item{GMT.offset}{offset from GMT time.}
  \item{decimation.method}{see online docs at site mentioned in References.}
  \item{reference.offset}{a reference offset; see online docs at site mentioned in References.}
  \item{reference.code}{a reference code; see online docs at site mentioned in References.}
  \item{units}{unit of observations (normally "MM")}
  \item{n}{number of observations}
  \item{data}{a list containing \code{t}, times of observations and \code{eta}, the sealevel observations, in units \code{units} (normally, mm)}
  \item{processing.log}{a processing log.}
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
h <- read.sealevel("ftp://ilikai.soest.hawaii.edu/rqds/pacific/monthly/m652a.dat")
summary(h)
plot(h)
}
}

\references{The Hawaii archive site at \url{http://ilikai.soest.hawaii.edu/uhslc/datai.html}
provides a graphical interface for downloading sealevel data; the format is described at
\url{ftp://ilikai.soest.hawaii.edu/rqds/hourly.fmt}. The MEDS data are at
\url{http://www.meds-sdmm.dfo-mpo.gc.ca/meds/Databases/TWL/TWL_inventory_e.htm}.}

\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}

\keyword{misc}
