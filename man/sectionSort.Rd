\name{sectionSort}

\alias{sectionSort}

\title{Sort a section}

\description{Sort a section}

\usage{sectionSort(section, by=c("stationId", "distance"))}

\arguments{
  \item{section}{a \code{section} object containing the section whose stations
      are to be sorted}
  \item{by}{a string indicating how to reorder.  Note that the
    \code{"distance"} method does not yet work ... also, eventually there
    will be other possibilities, e.g. \code{"latitude"}.}
}

\details{Sections created with \code{\link{makeSection}} have
"stations" that are in the order of the CTD objects (or filenames for
such objects) provided.  Sometimes, this is not the desired order,
e.g. if file names discovered with \code{\link{dir}} are in an order
that makes no sense.  (For example, a practioner might name stations
\code{"stn1"}, \code{"stn2"}, etc., not realizing that this will yield
an unhelpful ordering, by file name, if there are more than 9 stations.)

The purpose of \code{sectionSort} is to permit reordering the
constituent stations in sensible ways.
}

\value{An object of \code{\link[base]{class}} \code{"section"} that
has less lateral variation than the input section.}

\seealso{The documentation for \code{\link{section-class}} explains the
    structure of section objects, and also outlines the other functions dealing
    with them.}

\examples{
# Eastern North Atlantic, showing Mediterranean water. 
library(oce)
data(coastlineWorld)
data(a03)
s <- sectionGrid(subset.oce(a03, indices=90:3), p=seq(00, 2500, 100))
ss <- sectionSort(s, by="stationId")
plot(ss,coastline=coastlineWorld,map.xlim=c(-75,0))
}

\author{Dan Kelley}

\keyword{misc}
