\name{sectionSort}

\alias{sectionSort}

\title{Sort a section}

\description{Sort a section}

\usage{sectionSort(section, by)}

\arguments{
  \item{section}{a \code{section} object containing the section whose stations
      are to be sorted}

  \item{by}{an optional string indicating how to reorder.  If not provided,
      \code{"stationID"} will be assumed.  Other choices are \code{"distance"},
      for distance from the first station, \code{"longitude"}, for longitude,
      and \code{"latitude"} for latitude.}
}

\details{Sections created with \code{\link{as.section}} have
"stations" that are in the order of the CTD objects (or filenames for
such objects) provided.  Sometimes, this is not the desired order,
e.g. if file names discovered with \code{\link{dir}} are in an order
that makes no sense.  (For example, a practioner might name stations
\code{"stn1"}, \code{"stn2"}, etc., not realizing that this will yield
an unhelpful ordering, by file name, if there are more than 9 stations.)

The purpose of \code{sectionSort} is to permit reordering the
constituent stations in sensible ways.
}

\value{An object of \code{\link[base]{class}} \code{"section"} that has less
    lateral variation than the input section.}

\seealso{The documentation for \code{\link{section-class}} explains the
    structure of section objects, and also outlines the other functions dealing
    with them.}

\examples{
\dontrun{
# Eastern North Atlantic, showing Mediterranean water;
# sorting by longitude makes it easier to compare
# the map and the section.
library(oce)
data(section)
s <- sectionGrid(subset(section, -30 <= longitude))
ss <- sectionSort(ss, by="longitude")
plot(ss)
}
}

\author{Dan Kelley}

\keyword{misc}
