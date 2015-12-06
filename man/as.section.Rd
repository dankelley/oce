% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2:spell
\name{as.section}

\alias{as.section}

\title{Create a section}

\description{Create a section based on columnar data, a group of
  \code{\link{ctd-class}} objects, or an \code{\link{argo-class}} object.}

\usage{as.section(salinity, temperature, pressure, longitude, latitude, station,
  sectionId="")}

\arguments{
  \item{salinity}{This may be a numerical vector, in which case it is
    interpreted as the salinity, and the other arguments are used for the other
    components of \code{\link{ctd-class}} objects. Alternatively, it may be one
    of a variety of other objects from which the CTD objects can be inferred, in
    which case the other arguments are ignored; see \sQuote{Details}.}
  \item{temperature}{Temperature, in a vector holding values for all stations.}
  \item{pressure}{Pressure, in a vector holding values for all stations.}
  \item{longitude}{Longitude, in a vector holding values for all stations.}
  \item{latitude}{Latitude, in a vector holding values for all stations.}
  \item{station}{Station identifiers, in a vector holding values for all stations.}
  \item{sectionId}{section identifier.}
}

\details{If the first argument is a numerical vector, then it is taken to be the
  salinity, and \code{\link{factor}} is applied to \code{station} to break the
  data up into chunks that are assembled into \code{\link{ctd-class}} objects
  with \code{\link{as.ctd}} and combined to make a \code{\link{section-class}}
  object to be returned. This mode of operation is provided as a convenience for
  datasets that are already partly processed; if original CTD data are
  available, the next mode is preferred, because it permits the storage of much
  more data and metadata in the CTD object.

  If the first argument is a list containing oce objects that can be coerced
  into CTD form with \code{\link{as.ctd}} -- or a character vector containing
  the names of such objects that are defined in the calling environment -- then
  those objects are combined to form the station, and all other arguments are
  ignored.  This is the best way to call \code{as.section}.

  If the first argument is a \code{\link{argo-class}} object, then the profiles
  it contains are turned into \code{\link{ctd-class}} object, and these are
  assembled into a section to be returned.

}

\value{An object of \code{\link[base]{class}} \code{"section"}.}

\seealso{The documentation for \code{\link{section-class}} explains the
  structure of section objects, and outlines other functions dealing with them.}

\examples{
library(oce)
data(ctd)
## vector of names of CTD objects
fake <- ctd
fake[["temperature"]] <- ctd[["temperature"]] + 0.5
fake[["salinity"]] <- ctd[["salinity"]] + 0.1
fake[["longitude"]] <- ctd[["longitude"]] + 0.01
fake[["station"]] <- "fake"
sec1 <- as.section(c("ctd", "fake"))
summary(sec1)
## vector of CTD objects
ctds <- vector("list", 2)
ctds[[1]] <- ctd
ctds[[2]] <- fake
sec2 <- as.section(ctds)
summary(sec2)
## argo data (a subset)
data(argo)
sec3 <- as.section(subset(argo, profile<5))
summary(sec3)
}

\author{Dan Kelley}

\keyword{misc}
