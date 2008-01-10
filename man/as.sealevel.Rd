\name{as.sealevel}
\alias{as.sealevel}
\title{Coerce data into sea-level dataset}
\description{
  Coerces a dataset (minimally, a sequence of times and heights) into
  a sealevel dataset.
}
\usage{
as.sealevel(eta,
  t,
  header=NULL,
  station.number=NA,
  station.version=NA,
  station.name=NULL,
  region=NA,
  year=NA,
  latitude=NA,
  longitude=NA,
  GMT.offset=NA,
  decimation.method=NA,
  reference.offset=NA,
  reference.code=NA,
  sampling.interval=NA)
}
\arguments{
  \item{eta}{a list of sea-level heights in metres, in an hourly sequence.}
  \item{t}{optional list of times, in POSIXct format.  If missing, the list will be constructed assuming hourly samples, starting at 1AM on January first of the present calendar year.}
  \item{header}{a character string as read from first line of a standard data file.}
  \item{station.number}{three-character string giving station number.}
  \item{station.version}{single character for version of station.}
  \item{station.name}{the name of station (at most 18 characters).}
  \item{region}{the name of the region or country of station (at most 19 characters).}
  \item{year}{the year of observation.}
  \item{latitude}{the latitude in decimal degrees, positive north of the equator.}
  \item{longitude}{the longitude in decimal degrees, positive east of Greenwich.}
  \item{GMT.offset}{offset from GMT, in hours.}
  \item{decimation.method}{a coded value, with 1 meaning filtered, 2 meaning a simple average of all samples, 3 meaning spot readings, and 4 meaning some other method.}
  \item{reference.offset}{?}
  \item{reference.code}{?}
  \item{sampling.interval}{optional interval between samples, in hours; if not provided, 1 hour is assumed.}
}
\details{
  The arguments are based on the standard data format, as described
  at \url{ftp://ilikai.soest.hawaii.edu/rqds/hourly.fmt}.
}

\value{A \code{sealevel} object (see \code{\link{read.sealevel}} for details).}

\seealso{
  \code{\link{read.sealevel}} reads data,
  \code{\link{summary.sealevel}} summarizes the information, while
  \code{\link{plot.sealevel}} plots it.
}
\examples{
library(oce)
h <- 0:(24*100) # construct a 100-day series
t <- as.POSIXct("2001-09-11") + h * 3600
eta <- 1.0 * sin(2*pi*h/12.4172) + 0.8 * sin(2*pi*h/24.0)
eta <- eta + 0.1 * rnorm(length(h)) # add some noise
sl <- as.sealevel(eta, t)
summary(sl)
}
\references{
  \url{ftp://ilikai.soest.hawaii.edu/rqds/hourly.fmt}.
}
\author{Dan Kelley}
\keyword{misc}
