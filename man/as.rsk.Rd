% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{as.rsk}

\alias{as.rsk}

\title{Create a rsk object}

\description{Create a rsk object.}

\usage{as.rsk(time, columns,
       filename="", instrumentType="rbr", serialNumber="", model="",
       sampleInterval=NA,
       debug=getOption("oceDebug"))}

\arguments{
  \item{time}{a vector of times for the data.}
  \item{columns}{a list or data frame containing the measurements at the
    indicated times; see \dQuote{Details}.}
  \item{filename}{optional name of file containing the data}
  \item{instrumentType}{type of instrument}
  \item{serialNumber}{serial number for instrument}
  \item{model}{instrument model type, e.g. \code{"RBRduo"}}
  \item{sampleInterval}{sampling interval. If given as \code{NA}, then this 
    is estimated as the median difference in times.}
  \item{debug}{a flag that can be set to \code{TRUE} to turn on debugging.}
}

\details{The contents of \code{columns} are be copied into the \code{data} slot
  of the returned object directly, so it is critical that the names and units
  correspond to those expected by other code dealing with
  \code{\link{rsk-class}} objects. If there is a conductivity, it must be called
  \code{conductivity}, and it must be in units of mS/cm. If there is a
  temperature, it must be called \code{temperature}, and it must be an in-situ
  value recorded in ITS-90 units.  And if there is a pressure, it must be
  \emph{absolute} pressure (sea pressure plus atmospheric pressure) and it must
  be named \code{pressure}. No checks are made within \code{as.rsk} on any of
  these rules, but if they are broken, you may expect problems with any further
  processing.}

\value{An object of \code{\link[base]{class}} \code{"rsk"}.}

\seealso{The documentation for \code{\link{rsk-class}} explains the structure of
  \code{rsk} objects, and also outlines the other functions dealing with them.}

\author{Dan Kelley}

\keyword{misc}
