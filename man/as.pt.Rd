\name{as.pt}

\alias{as.pt}

\title{Create an TDR temperature-depth recorder data file}

\description{Create an pt temperature-depth recorder file, producing an object of type \code{pt}.}

\usage{ as.pt(time, temperature, pressure,
      filename="", instrumentType="rbr", serialNumber="",
      history, debug=getOption("oceDebug"))}

\arguments{
  \item{time}{a vector of times for the data.}
  \item{temperature}{temperatures at the give times.}
  \item{pressure}{pressures at the give times.}
  \item{filename}{optional name of file containing the data}
  \item{instrumentType}{type of instrument}
  \item{serialNumber}{serial number for instrument}
  \item{history}{if provided, the action item to be stored in the
    log.  (Typically only provided for internal calls; the default that
    it provides is better for normal calls by a user.)}
  \item{debug}{a flag that can be set to \code{TRUE} to turn on debugging.}
}

\details{This is used by \code{\link{read.pt}} to create \code{pt} objects.}  

\value{An object of \code{\link[base]{class}} \code{"pt"}, which is a
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
  \item{history}{a history of processing, in the standard \code{oce} format.}
}

\seealso{This is used by \code{\link{read.pt}}, and the help for the latter
   function lists other relevant functions.}

\author{Dan Kelley}

\keyword{misc}
