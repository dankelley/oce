\name{section.sort}

\alias{section.sort}

\title{Sort a section}

\description{Sort a section}

\usage{section.sort(section, by=c("station.id", "distance"))}

\arguments{
  \item{section}{a \code{section} object containing the section to be gridded.}
  \item{by}{a string indicating how to reorder.  Note that the
    \code{"distance"} method does not yet work ... also, eventually there
    will be other possibilities, e.g. \code{"latitude"}.}
}

\details{Sections created with \code{\link{make.section}} have
"stations" that are in the order of the CTD objects (or filenames for
such objects) provided.  Sometimes, this is not the desired order,
e.g. if file names discovered with \code{\link{dir}} are in an order
that makes no sense.  (For example, a practioner might name stations
\code{"stn1"}, \code{"stn2"}, etc., not realizing that this will yield
an unhelpful ordering, by file name, if there are more than 9 stations.)

The purpose of \code{section.sort} is to permit reordering the
constituent stations in sensible ways.
}

\value{An object of \code{\link[base]{class}} \code{"section"} that
has less lateral variation than the input section.}

\seealso{Sections are normally created with \code{\link{read.section}}
  or \code{\link{make.section}}.  \code{\link{subset}} may be
  used to create a new section consisting of just a subset of an
  existing station, e.g. to focus on a particular region.}

\examples{
# Eastern North Atlantic, showing Mediterranean water. 
library(oce)
data(coastline.world)
data(a03)
s <- section.grid(subset(a03, indices=90:3), p=seq(00, 2500, 100))
ss <- section.sort(s, by="station.id")
plot(ss,coastline=coastline.world,map.xlim=c(-75,0))
}

\author{Dan Kelley}

\keyword{misc}
