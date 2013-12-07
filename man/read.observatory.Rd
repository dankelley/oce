\name{read.observatory}

\alias{read.observatory}
\alias{read.observatory.ctd}

\title{Read observatory data file}

\description{Read observatory data file, e.g. produces by VENUS}

\usage{read.observatory(file, type=c("ctd"),
                debug=getOption("oceDebug"), processingLog, ...)
read.observatory.ctd(file, 
                     debug=getOption("oceDebug"), processingLog, ...)
}

\arguments{
  \item{file}{a connection or a character string giving the name of the file to
      load.}
  \item{type}{type of data; must be \code{"ctd"}, the only type handled at
      present.}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
    moderate amount of debugging information, or to 2 to get more.} 
  \item{processingLog}{if provided, the action item to be stored in the
    log.  (Typically only provided for internal calls; the default that
    it provides is better for normal calls by a user.)}
  \item{\dots}{additional arguments, passed to called routines.}
}

\details{These are a preliminary versions of functions that may be extended or
    deleted in a future version of Oce.  Part of the uncertainty is that the
    format was inferred from inspection of files, and this is always a
    dangerous path (compared to following lear documentation on file formats)
    because the data archiver might alter formats in future.}

\value{An object of \code{\link[base]{class}} \code{"ctd"}.}

\seealso{The documentation for \code{\link{ctd-class}} explains the structure
    of \code{ctd} objects, and also outlines the other functions dealing with
    them.}

\examples{
library(oce)
\dontrun{
ctd <- read.oce("data.txt")
## mooring data are not profiles, so show timeseries
par(mfrow=c(3,1))
oce.plot.ts(ctd[["time"]], ctd[["pressure"]])
oce.plot.ts(ctd[["time"]], ctd[["salinity"]])
oce.plot.ts(ctd[["time"]], ctd[["temperature"]])
}
}

\references{\url{http://venus.uvic.ca/data/data-archive/}}

\author{Dan Kelley}

\keyword{misc}
