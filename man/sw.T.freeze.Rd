\name{sw.T.freeze}

\alias{sw.T.freeze}

\title{Seawater freezing temperature}

\description{Compute freezing temperature of seawater.}

\usage{sw.T.freeze(salinity, pressure=NULL)}

\arguments{
  \item{salinity}{either salinity [PSU] or a \code{ctd} object from which
  salinity will be inferred.}
  \item{pressure}{seawater pressure [dbar]}
}

\details{In the first form, the argument is a \code{ctd} object, from
  which the salinity and pressure values are extracted and used to for
  the calculation.}

\value{Temperature [\eqn{^\circ}{deg}C]}

\examples{
 T.freeze <-  sw.T.freeze(40, 500) # -2.588567 degC
}

\references{UNESCO tech. papers in the marine science no. 28. 1978
  eighth report JPOTS Annex 6 freezing point of seawater F.J. Millero
  pp.29-35.}

\author{Dan Kelley}

\keyword{misc}
