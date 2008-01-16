\name{gravity}
\alias{gravity}
\title{Acceleration due to earth gravity}
\description{
Compute \eqn{g}{g}, the acceleration due to gravity, as a function of latitude.
}
\usage{
  g <- gravity(lat, degrees=TRUE);
}
\arguments{
  \item{lat}{Latitude in \eqn{^\circ}{deg}N or radians north of the equator.}
  \item{degrees}{Flag indicating whether degrees are used for latitude;
    if set to \code{FALSE}, radians are used.}
}

\details{Value not verified yet, except roughly.}

\value{Acceleration due to gravity [\eqn{m^2/s}{m^2/s}].}

\seealso{N/A.}

\examples{
 g <- gravity(45) # 9.8
}
\references{
 Gill, A.E., 1982. \emph{Atmosphere-ocean Dynamics}, 
 Academic Press, New York, 662 pp.

 \strong{Caution:} Fofonoff and Millard (1983 UNESCO) use a different
formula.
}
\author{Dan Kelley}
\keyword{misc}
