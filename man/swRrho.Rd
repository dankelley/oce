\name{swRrho}

\alias{swRrho}

\title{Density ratio}

\description{Compute density ratio}

\usage{swRrho(ctd, sense=c("diffusive", "finger"), smoothingLength=10, df)}

\arguments{

  \item{ctd}{an object of class \code{ctd}}

  \item{sense}{an indication of the sense of double diffusion under study and
      therefore of the definition of Rrho; see \sQuote{Details}}

  \item{smoothingLength}{ignored if \code{df} supplied, but otherwise the
      latter is calculated as the number of data points, divided by the number
      within a depth interval of \code{smoothingLength} metres.}
  
  \item{df}{if given, this is provided to \code{\link{smooth.spline}}.}

}
  
\details{This computes Rrho (density ratio) from a \code{ctd} object,
    calculating derivatives from smoothing splines whose properties are
    governed by \code{smoothingLength} or \code{df}.  If
    \code{sense="diffusive"} the definition is
    \eqn{(beta*dS/dz)/(alpha*d(theta)/dz)}{(beta*dS/dz)/(alpha*d(theta)/dz)}
    and the reciprocal for \code{"finger"}.

    If the default arguments are acceptable, \code{ctd[["Rrho"]]} may be used
    instead of \code{swRrho(ctd)}.

} 

\value{Density ratio defined in either the \code{"diffusive"} or
    \code{"finger"} sense.}

\examples{
library(oce)
data(ctd)
plot(swRrho(ctd), ctd[["depth"]])
}

\author{Dan Kelley and Chantelle Layton}

\keyword{misc}
