\name{coriolis}
\alias{coriolis}
\title{Coriolis parameter on rotating earth}
\description{
Compute \eqn{f}{f}, the Coriolis parameter as a function of latitude.
}
\usage{
  f <- coriolis(lat, degrees=TRUE);
}
\arguments{
  \item{lat}{Latitude in \eqn{^\circ}{deg}N or radians north of the equator.}
  \item{degrees}{Flag indicating whether degrees are used for latitude;
    if set to \code{FALSE}, radians are used.}
}
\details{
  
}
\value{Coriolis parameter [radian/s].}

\seealso{
  N/A.
}
\examples{
 C <- coriolis(45) # 1e-4
}
\references{
 Gill, A.E., 1982. \emph{Atmosphere-ocean Dynamics}, 
 Academic Press, New York, 662 pp.
}
\author{Dan Kelley}
\keyword{misc}
