\name{read.section}
\alias{read.section}
\title{Read a section containing multiple CTD profiles}
\description{Read a file that contains a series of \code{ctd} profiles that make up an oceanographic section.}
\usage{read.section(file, section.id, debug=FALSE)}
\arguments{
  \item{file}{a file containing a set of CTD observations.  At present, only the \emph{exchange BOT} format is accepted (see Details).}
  \item{section.id}{optional string indicating the name for the section.  If not provided, the section ID is determined by examination of the file header.}
  \item{debug}{logical. If \code{TRUE}, print some information that might be helpful during debugging.}
}

\details{Only \emph{exchange BOT} comma-separated value format is
permitted at this time, but other formats may be added later.  It
should also be noted that the parsing scheme was developed after
inspection of the A03 data set (see Examples). This may cause problems
if the format is not universal. For example, the header must name the
salinity column "\code{CTDSAL}"; if not, salinity values will not be
read from the file.}
\value{
A \code{section} object, which is a list containing the following items
\item{header}{the header from the data file.}
\item{section.id}{a name for the section.}
\item{station.id}{a vector of station IDs, one per station.}
\item{latitude}{a vector of station latitudes, decimal and positive in northern hemisphere.}
\item{longitude}{a vector of station latitudes, decimal and positive in eastern hemisphere.}
\item{station}{a list of \code{ctd} objects (see e.g. \code{\link{read.ctd}}), one per station.}
\item{processing.log}{a processing log.}
}

\seealso{\code{\link{make.section}} can be used to bind together \code{ctd} objects into a section.}
\examples{
library(oce)
data(a03)
Gulf.Stream <- section.subset(a03, 124:102)
Gulf.Stream.gridded <- section.grid(Gulf.Stream, p=seq(0, 5000, 10))
data(coastline.world)
plot(Gulf.Stream.gridded, coastline=coastline.world, map.xlim=c(-80,-60))
}
\references{
Several repository sites provide section data. An example that is perhaps likely
to exist for years is \url{http://cchdo.ucsd.edu}, but a search on \code{"WOCE
bottle data"} should turn up other sites, if this one ceases to exist. Only the
so-called \emph{exchange BOT} data format can be processed by read.section() at
this time.
}
\author{Dan Kelley}
\keyword{misc}
