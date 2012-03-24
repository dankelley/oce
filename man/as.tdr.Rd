\name{as.tdr}

\alias{as.tdr}

\title{Create a TDR object}

\description{Create a TDR (temperature-depth recorder) object.}

\usage{as.tdr(time, temperature, pressure,
      filename="", instrumentType="rbr", serialNumber="", model="", pressureAtmospheric=NA,
      processingLog, debug=getOption("oceDebug"))}

\arguments{
  \item{time}{a vector of times for the data.}
  \item{temperature}{temperatures at the give times.}
  \item{pressure}{pressures at the give times.}
  \item{filename}{optional name of file containing the data}
  \item{instrumentType}{type of instrument}
  \item{serialNumber}{serial number for instrument}
  \item{model}{instrument model type, e.g. \code{"RBRduo"}}
  \item{pressureAtmospheric}{optional atmospheric pressure, in the same unit as seawater pressure}
  \item{processingLog}{if provided, the action item to be stored in the
    log.  (Typically only provided for internal calls; the default that
    it provides is better for normal calls by a user.)}
  \item{debug}{a flag that can be set to \code{TRUE} to turn on debugging.}
}

\details{This is used by \code{\link{read.tdr}} to create \code{tdr} objects.}  

\value{An object of \code{\link[base]{class}} \code{"tdr"}, which is a
  list with elements detailed below.
  \item{data}{a data table containing the \code{time},
    \code{temperature}, and \code{pressure} data.}
  \item{metadata}{a list containing the following items
    \describe{
      \item{\code{header}}{the header itself, as read from the input file.}
      \item{\code{serialNumber}}{serial number of instrument, inferred
	from first line of the header.}
      \item{\code{loggingStart}}{start time for logging, inferred from
	the header.  Caution: this is often not the first time in the
	data, because the data may have been subsetted.}
      \item{\code{samplePeriod}}{seconds between samples, inferred from
	the header.  Caution: this is often not the sampling period in the
	data, because the data may have been decimated.}
    }
  }
  \item{processingLog}{a processingLog of processing, in the standard \code{oce} format.}
}

\seealso{The documentation for \code{\link{tdr-class}} explains the structure of
    \code{tdr} objects, and also outlines the other functions dealing with
    them.}

\author{Dan Kelley}

\keyword{misc}
