\name{swPressure}

\alias{swPressure}

\title{Water pressure}

\description{Compute seawater pressure from depth by inverting
    \code{\link{swDepth}} using \code{\link{uniroot}}.}

\usage{swPressure(depth , latitude=45, eos=getOption("oceEOS", default="gsw"))}

\arguments{
  \item{depth}{distance below the surface in metres.}
  \item{latitude}{Latitude in \eqn{^\circ}{deg}N or radians north of the equator.}
  \item{eos}{indication of formulation to be used, either \code{"unesco"} or
      \code{"gsw"}.}
}

\details{If \code{eos="unesco"} this is done by numerical inversion of
    \code{\link{swDepth}} is done using \code{\link{uniroot}}. If
    \code{eos="gsw"}, it is done using \code{\link[gsw]{gsw_p_from_z}} in the
    \CRANpkg{gsw} package.}

\value{Pressure in dbar.}

\examples{
swPressure(9712.653, 30, eos="unesco") # 10000
swPressure(9712.653, 30, eos="gsw")    #  9998.863
}

\references{Unesco 1983. Algorithms for computation of fundamental
      properties of seawater, 1983. \emph{Unesco Tech. Pap. in
      Mar. Sci.}, No. 44, 53 pp.}

\author{Dan Kelley}

\keyword{misc}
