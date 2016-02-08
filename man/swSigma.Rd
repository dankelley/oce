% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{swSigma}

\alias{swSigma}

\title{Seawater density anomaly}

\description{Compute \eqn{\sigma_\theta}{sigma}, the density of seawater, minus
  1000 kg/m\eqn{^3}{^3}.}

\usage{swSigma(salinity, temperature=NULL, pressure=NULL, longitude=300,
  latitude=30, eos=getOption("oceEOS", default="gsw"))}

\arguments{
  \item{salinity}{either practial salinity or an \code{oce} object; see
    \dQuote{Details}.}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C], defined on
    the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
    \code{\link{swRho}}, and the examples below.}
  \item{pressure}{pressure [dbar].}
  \item{longitude}{longitude of observation (only used if \code{eos="gsw"}).}
  \item{latitude}{latitude of observation (only used if \code{eos="gsw"}).}
  \item{eos}{name of equation of state to be used, either \code{"unesco"} or
      \code{"gsw"}.}
}

\details{If the first argument is an \code{oce} object, then salinity, etc., are
  extracted from it, and used for the calculation instead of any values provided
  in the other arguments.}

\value{Density anomaly [kg/m\eqn{^3}{^3}], defined as \code{\link{swRho}} - 1000
  kg/m\eqn{^3}{^3}.}

\seealso{Related density routines include: \code{\link{swRho}},
  \code{\link{swSigmaT}}, \code{\link{swSigmaTheta}}, \code{\link{swSigma0}},
  etc.}

\examples{
library(oce)
swSigma(35, 13, 1000, eos="gsw")                   # 30.82397
swSigma(35, T90fromT68(13), 1000, eos="unesco")    # 30.8183
}

\references{See citations provided in the \code{\link{swRho}} documentation.}

\author{Dan Kelley}

\keyword{misc}
