\name{sw.alpha}

\alias{sw.alpha}

\title{Seawater thermal expansion coefficient}

\description{Compute \eqn{\alpha}{alpha}, the seawater thermal expansion
  coefficient, as the product of \eqn{\alpha/\beta}{alpha/beta} and
  \eqn{\beta}{beta}, each calculated from McDougall's (1987) algorithm.}

\usage{sw.alpha(S, t=NULL, p=NULL, is.theta=FALSE)}

\arguments{
  \item{S}{either salinity [PSU] (in which case \code{t} and \code{p}
    must be provided) \strong{or} a \code{ctd} object (in which case
    \code{S}, \code{t} and \code{p} are determined from the object, and
    must not be provided in the argument list).}
  \item{t}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{p}{pressure [dbar]}
  \item{is.theta}{Set TRUE if t is theta or FALSE if t is in-situ}
}

\details{If the first argument is a \code{ctd} object, then salinity,
  temperature and pressure values are extracted from it, and used for
  the calculation.}

\value{Value in 1/degC.}

\examples{
 # 2.5060e-4 (inferred from p1964 of McDougall 1987)
 a <- sw.alpha(40, 10, 4000) 
}

\references{McDougall, T.J. 1987. "Neutral Surfaces" Journal of Physical
  Oceanography vol 17 pages 1950-1964}

\author{Dan Kelley}

\keyword{misc}
