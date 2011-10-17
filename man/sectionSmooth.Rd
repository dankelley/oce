\name{sectionSmooth}

\alias{sectionSmooth}

\title{Smooth a section (preliminary version that may change)}

\description{Smooth a section in the lateral (alpha version that may change)}

\usage{sectionSmooth(section, df, debug=getOption("oceDebug"), ...)}

\arguments{
  \item{section}{a \code{section} object containing the section to be smoothed}
  \item{df}{optional value for \code{df}, used by
    \code{\link{smooth.spline}}.  If not supplied, this is set to the
    number of stations, divided by 5.}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
    moderate amount of debugging information, or to 2 to get more.}
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
arguments are supplied, \code{sectionSmooth} will set \code{df} to
the number of stations divided by 5, which could be interpreted as
smoothing across 5 adjacent stations.

}

\value{An object of \code{\link[base]{class}} \code{"section"} that
has less lateral variation than the input section.}

\seealso{Sections are normally created with \code{\link{read.section}} or
    \code{\link{makeSection}}, and gridded with \code{\link{sectionGrid}}.
    The generics \code{subset}, \code{plot} and \code{summary} methods work,
    also.}

\examples{
# Eastern North Atlantic, showing Mediterranean water. 
library(oce)
data(coastlineWorld)
data(a03)
s <- sectionGrid(subset.oce(a03, indices=90:3), p=seq(00, 2500, 100))
ss <- sectionSmooth(s, df=8)
plot(ss,coastline=coastlineWorld,map.xlim=c(-75,0))
}

\author{Dan Kelley}

\keyword{misc}
