\name{read.gps}

\alias{read.gps}

\title{Scan a gps data file}

\description{Read a gps file in gpx format format.}

\usage{read.gps(file, type, debug=getOption("oceDebug"), processingLog)}

\arguments{
  \item{file}{name of file containing gps data.}
  \item{type}{type of file, which will be inferred from examination of the data
      if not supplied.  In the present version, the only choice for \code{type}
      is \code{"gpx"}.}
  \item{debug}{set to TRUE to print information about the header, etc.}
  \item{processingLog}{if provided, the action item to be stored in the
    log.  (Typically only provided for internal calls; the default that
    it provides is better for normal calls by a user.)}
}

\details{Reads GPX format files by simply finding all longitudes and latitudes;
    in other words, information on separate tracks, or waypoints, etc., is
    lost.}

\value{An object of \code{\link[base]{class}} \code{"gps"}.} 

\seealso{The documentation for \code{\link{gps-class}} explains the structure
    of gps objects, and also outlines the other functions dealing with them.}

\examples{
\dontrun{
library(oce)
gps <- read.gps("~/Dropbox/dalwhoi.gpx")
}
}

\references{The GPX format is described at \url{http://www.topografix.com/GPX/1/1/}.}

\author{Dan Kelley}

\keyword{misc}
