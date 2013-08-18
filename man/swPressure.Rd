\name{swPressure}

\alias{swPressure}

\title{Water pressure}

\description{Compute seawater pressure from depth by inverting
    \code{\link{swDepth}} using \code{\link{uniroot}}.}

\usage{
swPressure(depth , latitude=45)
}

\arguments{
  \item{depth}{distance below the surface in metres.}
  \item{latitude}{Latitude in \eqn{^\circ}{deg}N or radians north of the equator.}
}

\details{Numerical inversion of \code{\link{swDepth}} is done using
    \code{\link{uniroot}}.}

\value{Pressure in dbar.}

\examples{
swPressure(9712.653, 30) # should be 10km (check value in UNESCO 1983).
}

\references{Unesco 1983. Algorithms for computation of fundamental
      properties of seawater, 1983. \emph{Unesco Tech. Pap. in
      Mar. Sci.}, No. 44, 53 pp.}

\author{Dan Kelley}

\keyword{misc}
