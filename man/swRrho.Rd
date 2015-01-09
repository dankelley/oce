\name{swRrho}

\alias{swRrho}

\title{Density ratio}

\description{Compute density ratio}

\usage{swRrho(ctd, sense=c("diffusive", "finger"), smoothingLength=10, df,
       eos=getOption("oceEOS", default="gsw"))}

\arguments{

  \item{ctd}{an object of class \code{ctd}}

  \item{sense}{an indication of the sense of double diffusion under study and
      therefore of the definition of Rrho; see \sQuote{Details}}

  \item{smoothingLength}{ignored if \code{df} supplied, but otherwise the
      latter is calculated as the number of data points, divided by the number
      within a depth interval of \code{smoothingLength} metres.}
  
  \item{df}{if given, this is provided to \code{\link{smooth.spline}}.}

  \item{eos}{equation of state, either \code{"unesco"} or \code{"gsw"}.}

}
  
\details{This computes Rrho (density ratio) from a \code{ctd} object.  
    
    If \code{eos="unesco"}, this is done by calculating salinity and
    potential-temperature derivatives from smoothing splines whose properties
    are governed by \code{smoothingLength} or \code{df}.  If
    \code{sense="diffusive"} the definition is
    \eqn{(beta*dS/dz)/(alpha*d(theta)/dz)}{(beta*dS/dz)/(alpha*d(theta)/dz)}
    and the reciprocal for \code{"finger"}.

    If \code{eos="gsw"}, this is done by extracting absolute salinity and
    conservative temperature, smoothing with a smoothing spline as in the
    \code{"unesco"} case, and then calling
    \code{\link[gsw]{gsw_Turner_Rsubrho}} on these smoothed fields. Since the
    gsw function works on mid-point pressures, \code{\link{approx}} is used to
    interpolate back to the original pressures.

    If the default arguments are acceptable, \code{ctd[["Rrho"]]} may be used
    instead of \code{swRrho(ctd)}.

} 

\value{Density ratio defined in either the \code{"diffusive"} or
    \code{"finger"} sense.}

\examples{
library(oce)
data(ctd)
u <- swRrho(ctd, eos="unesco")
g <- swRrho(ctd, eos="gsw")
p <- ctd[["p"]]
plot(u, p, ylim=rev(range(p)), type='l', xlab=expression(R[rho]))
lines(g, p, lty=2, col='red')
legend("topright", lty=1:2, legend=c("unesco", "gsw"), col=c("black", "red"))
}

\author{Dan Kelley and Chantelle Layton}

\keyword{misc}
