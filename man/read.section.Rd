\name{read.section}
\alias{read.section}
\title{Read a section containing multiple CTD profiles}
\description{Read a file that contains a series of CTD profiles that make up an oceanographic section.}
\usage{read.section(file, debug=FALSE)}
\arguments{
  \item{file}{A file containing a set of CTD observations.  At present, only the ``exchange BOT'' format is accepted (see references).}
  \item{debug}{Set TRUE to print some information that might be helpful during debugging.}
}
\details{
Only ``exchange BOT'' comma-separated value format is permitted at this time, but other formats may be added later.

\emph{Bug 1:} the parsing is primitive, having been designed by inspection of the
A03 data set (see Examples). For example, it is assumed that the header names the
salinity column as \code{CTDSAL}, and if another name is used, no salinity will be
read from the file.  (Possibly this is not a problem; the author has just not checked yet on the strictness of the header format.)

\emph{Bug 2:} the routine does not read measurements other than temperature
(CTDTMP), salinity (CTDSAL) and pressure (CTDPRS). It should check for other
items, e.g. nutrients and tracers.
}
\value{
  A \code{section} object.  
}
\seealso{
  \code{\link{make.section}} can be used to manually bind together CTD objects into a section.
}
\examples{
\dontrun{
library(oce)
# http://cchdo.ucsd.edu/data_access?ExpoCode=90CT40_1
# ExpoCode 90CT40_1, Chief Scientist Tereschenkov/SOI
s <- read.section("a03_hy1.csv") 
plot(s)
summary(s)
}
}
\references{
Several repository sites provide section data. An example that is perhaps likely
to exist for years is \url{http://cchdo.ucsd.edu}, but a search on \code{"WOCE
bottle data"} should turn up other sites, if this one ceases to exist. Only the
so-called \emph{exchange BOT} data format can be processed by read.section() at
this time.
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}
