\name{makeSection}
\alias{makeSection}
\alias{+.section}
\title{Bind CTD profiles together into a cross section}
\description{Combine a series of CTD profiles together to create a section.}

\usage{
makeSection(item, \dots)
\special{section + station}
}

\arguments{
  \item{item}{either (1) a \code{ctd} object, in which case the rest of the
    arguments are other \code{ctd} objects, (2) a list of \code{ctd}
    objects, (3) a list of names of \code{ctd} objects, or (4) a list of
    names of files containing \code{ctd} data.}
  \item{\dots}{one or more \code{ctd} objects, either given as separate
    arguments, or a list of such objects.}
  \item{section}{a section to which a station is to be added.}
  \item{station}{a station to be added to a section.}
}

\details{The stations are stored in order of the station
  identification number (stored as \code{metadata$station} in the ctd
  object), if possible.  The \code{ctd} stations must share identical
  pressure values; use \code{\link{sectionGrid}} to do that.}

\value{An object of \code{\link[base]{class}} "\code{section}" (for
  details, see \code{\link{read.section}}).}

\seealso{\code{\link{read.ctd}} reads CTD data.  \code{\link[ocedata]{section}}
  is a sample data set.}

\examples{
library(oce)
data(ctd)
ctdWarmed <- ctd
ctdWarmed[["temperature"]] <- ctdWarmed[["temperature"]] + 0.5
ctdWarmed[["latitude"]] <- ctdWarmed[["latitude"]] + 0.1
section <- makeSection(ctd, ctdWarmed)
summary(section)

s2 <- makeSection(ctd)
s2 <- s2 + ctdWarmed
plot(s2)
# Below is how to create a section from well-named CSV files
# that contain consecutive stations
\dontrun{
    plot(sectionSmooth(sectionGrid(makeSection(dir("*.csv")))))
}
}

\author{Dan Kelley}

\keyword{misc}
