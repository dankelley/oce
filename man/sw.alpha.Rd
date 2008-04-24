\name{sw.alpha}

\alias{sw.alpha}

\title{Seawater thermal expansion coefficient}

\description{Compute \eqn{\alpha}{alpha}, the seawater thermal expansion
  coefficient, as the product of \eqn{\alpha/\beta}{alpha/beta} and
  \eqn{\beta}{beta}, each calculated from McDougall's (1987) algorithm.}

\usage{
a <- sw.alpha(ctd, is.theta=FALSE)
a <- sw.alpha(S, t=NULL, p=NULL, is.theta=FALSE)
}

\arguments{
  \item{ctd}{an object of \code{\link[base]{class}} "\code{ctd}".}
  \item{S}{salinity [PSU]}
  \item{t}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{is.theta}{Set TRUE if t is theta or FALSE if t is in-situ}
}

\details{In the first form, the argument is a \code{ctd} object, from
  which the salinity, temperature and pressure values are extracted and
  used to for the calculation.  Note the use of potential temperature,
  not temperature.}

\value{Value in 1/degC.}

\examples{
 # 2.5060e-4 (inferred from p1964 of McDougall 1987)
 a <- sw.alpha(40, 10, 4000) 
}

\references{McDougall, T.J. 1987. "Neutral Surfaces" Journal of Physical
  Oceanography vol 17 pages 1950-1964}

\author{Dan Kelley}

\keyword{misc}
