\name{section.smooth}

\alias{section.smooth}

\title{Smooth a section (preliminary version that may change)}

\description{Smooth a section in the lateral (alpha version that may change)}

\usage{section.smooth(s, ...)}

\arguments{
  \item{s}{a \code{section} object containing the section to be gridded.}
  \item{...}{optional arguments to be supplied to \code{\link{smooth.spline}}.}
}
\details{

\strong{This is a preliminary version of this function, and it may
change in future versions.  In particular, this function may get the
ability to smooth in the vertical also, and perhaps additional
smoothing methods should be allowed.}

The section is smoothed using \code{\link{smooth.spline}} on
individual pressure levels.  This is done for temperature, salinity,
and sigma-theta individually.  (\strong{This may change; perhaps
sigma-theta should be calculated from the smoothed temperature and
salinity.})

The \code{...} arguments will be passed to the spline routine,
allowing the user to control the spline as desired.  If no \code{...}
arguments are supplied, \code{section.smooth} will set \code{df} to
the number of stations divided by 5, which could be interpreted as
smoothing across 5 adjacent stations.

}

\value{An object of \code{\link[base]{class}} \code{"section"} that
has less lateral variation than the input section.}

\seealso{Sections are normally created with \code{\link{read.section}}
  or \code{\link{make.section}}.  \code{\link{section.subset}} may be
  used to create a new section consisting of just a subset of an
  existing station, e.g. to focus on a particular region.}

\examples{
# Eastern North Atlantic, showing Mediterranean water. 
library(oce)
data(coastline.world)
data(a03)
# Station 90 is south of Halifax, station 3 is near Med entrance
s <- section.grid(section.subset(a03, 90:3), p=seq(00, 2500, 100))
ss <- section.smooth(s, df=8)
plot(ss,coastline=coastline.world,map.xlim=c(-75,0))
}

\author{Dan Kelley}

\keyword{misc}
