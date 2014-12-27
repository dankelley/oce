\name{swTSrho}

\alias{swTSrho}

\title{Seawater temperature from salinity and density}

\description{Compute \emph{in-situ} temperature, given salinity, density, and pressure.}

\usage{swTSrho(salinity, density, pressure, eos=getOption("oceEOS", default="gsw"))}

\arguments{
  \item{salinity}{\emph{in-situ} salinity [PSU]}
  \item{density}{\emph{in-situ} density or sigma value [kg/m\eqn{^3}{^3}]}
  \item{pressure}{\emph{in-situ} pressure [dbar]}
  \item{eos}{name of equation of state to be used, either \code{"unesco"},
      \code{"gsw"} or \code{"teos"}.  In the second two cases, the \code{gsw}
      package must be installed.}
}

\details{Finds the temperature that yields the given density, with the
  given salinity and pressure.  The method is a bisection search with
  temperature tolerance 0.001 \eqn{^\circ C}{degC}.  }

\value{\emph{In-situ} temperature [\eqn{^\circ C}{degC}].}

\seealso{\code{\link{swSTrho}}}

\examples{
swTSrho(35, 23, 0) # 26.11301
swTSrho(35, 23, 0, eos="gsw") # 26.11301
swTSrho(35, 23, 0, eos="teos") # 26.11301
swTSrho(35, 23, 0, eos="unesco") # 26.11301
}

\references{Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for
 computation of fundamental properties of seawater. \emph{Unesco
 Technical Papers in Marine Science}, \bold{44}, 53 pp

Gill, A.E., 1982. \emph{Atmosphere-ocean Dynamics}, Academic Press, New
York, 662 pp. }

\author{Dan Kelley}

\keyword{misc}
