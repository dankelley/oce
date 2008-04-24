\name{sw.alpha.over.beta}

\alias{sw.alpha.over.beta}

\title{Ratio of seawater thermal expansion coefficient to haline contraction coefficient}

\description{Compute \eqn{\alpha/\beta}{alpha/beta} using McDougall's (1987) algorithm.}

\usage{
ab <- sw.alpha.over.beta(ctd, is.theta=FALSE);
ab <- sw.alpha.over.beta(S, t=NULL, p=NULL, is.theta=FALSE);
}

\arguments{
  \item{ctd}{an object of \code{\link[base]{class}} "\code{ctd}".}
  \item{S}{salinity [PSU]}
  \item{t}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{p}{pressure [dbar]}
  \item{is.theta}{Set TRUE if t is theta or FALSE if t is in-situ}
}

\details{In the first form, the argument is a \code{ctd} object, from
  which the salinity, temperature and pressure values are extracted and
  used to for the calculation. Note the use of potential temperature,
  not temperature.}

\value{Value in psu/degC.}

\examples{
 # 0.3476 (from p1964 of McDougall 1987)
 ab <- sw.alpha.over.beta(40, 10, 4000, is.theta=TRUE) 
}

\references{McDougall, T.J. 1987. "Neutral Surfaces" Journal of Physical
  Oceanography vol 17 pages 1950-1964}

\author{Dan Kelley}

\keyword{misc}
