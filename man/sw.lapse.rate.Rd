\name{sw.lapse.rate}

\alias{sw.lapse.rate}

\title{Seawater lapse rate}

\description{Compute adiabatic lapse rate}

\usage{sw.lapse.rate(S, t=NULL, p=NULL)}

\arguments{
  \item{S}{either salinity [PSU] (in which case \code{t} and \code{p}
    must be provided) \strong{or} a \code{ctd} object (in which case
    \code{S}, \code{t} and \code{p} are determined from the object, and
    must not be provided in the argument list).}
  \item{t}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{p}{pressure [dbar]}
}

\details{If the first argument is a \code{ctd} object, then salinity,
  temperature and pressure values are extracted from it, and used for
  the calculation.

  Compute lapse rate using Fofonoff and Millard's (1983) formula.
}

\value{Lapse rate [\eqn{deg}{deg}C/m].}

\examples{
 lr <- sw.lapse.rate(40, 40, 10000) # 3.255976e-4
}

\references{Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for
 computation of fundamental properties of seawater. \emph{Unesco
 Technical Papers in Marine Science}, \bold{44}, 53 pp.  (Section 7,
 pages 38-40) }
\author{Dan Kelley}

\keyword{misc}
