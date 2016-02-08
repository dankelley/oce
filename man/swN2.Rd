\name{swN2}

\alias{swN2}

\title{Squared buoyancy frequency for seawater}

\description{Compute 
\eqn{N^2}{N^2}, the square of the buoyancy frequency for a seawater profile.
}

\usage{swN2(pressure, sigmaTheta=NULL, derivs, df, eos=getOption("oceEOS", default="gsw"), \dots)}

\arguments{

  \item{pressure}{either pressure [dbar] (in which case \code{sigmaTheta} must
      be provided) \strong{or} an object of class \code{ctd} object (in which
      case \code{sigmaTheta} is inferred from the object.}

  \item{sigmaTheta}{Surface-referenced potential density minus 1000
      [kg/m\eqn{^3}{^3}]}

  \item{derivs}{optional argument to control how the derivative
      \eqn{d\sigma_\theta/dp}{d(sigmaTheta)/d(pressure)} is calculated.  This
      may be a character string or a function of two arguments.  See
      \dQuote{Details}.}

  \item{df}{argument passed to \code{\link{smooth.spline}} if this function is
      used for smoothing; set to \code{NA} to prevent smoothing.}

  \item{eos}{equation of state, either \code{"unesco"} or \code{"gsw"}.}

  \item{\dots}{additional argument, passed to \code{\link{smooth.spline}}, in
      the case that \code{derivs="smoothing"}.  See \dQuote{Details}.}

}
  
\details{If the first argument is a \code{ctd} object, then density is is
    inferred from it, and the \code{sigmaTheta} argument is ignored.

    Smoothing is often useful prior to computing buoyancy frequency, and so
    this may optionally be done with \code{\link{smooth.spline}}, unless
    \code{df=NA}, in which case raw data are used.  If \code{df} is not
    provided, a possibly reasonable value computed from an analysis of the
    profile, based on the number of pressure levels.

    If \code{eos="gsw"}, then the first argument must be a \code{ctd} object,
    and processing is done with \code{\link[gsw]{gsw_Nsquared}}, based on
    extracted values of Absolute Salinity and Conservative Temperature
    (possibly smoothed, depending on \code{df}).

    If \code{eos="unesco"}, then the processing is as follows.  The core of the
    method involves differentiating potential density (referenced to median
    pressure) with respect to pressure, and the \code{derivs} argument is used
    to control how this is done, as follows.

    \itemize{

        \item if \code{derivs} is not supplied, the action is as though it were
        given as the string \code{"smoothing"}

        \item if \code{derivs} equals \code{"simple"}, then the derivative of
        density with respect to pressure is calculated as the ratio of
        first-order derivatives of density and pressure, each calculated using
        \code{\link{diff}}.  (A zero is appended at the top level.)

        \item if \code{derivs} equals \code{"smoothing"}, then the processing
        depends on the number of data in the profile, and on whether \code{df}
        is given as an optional argument.  When the number of points exceeds 4,
        and when \code{df} exceeds 1, \code{\link{smooth.spline}} is used to
        calculate smoothing spline representation the variation of density as a
        function of pressure, and derivatives are extracted from the spline
        using \code{predict}.  Otherwise, density is smoothed using
        \code{\link{smooth}}, and derivatives are calculated as with the
        \code{"simple"} method.

        \item if \code{derivs} is a function taking two arguments (first
        pressure, then density) then that function is called directly to
        calculate the derivative, and no smoothing is done before or after that
        call.
    }

    For deep-sea work, the \code{eos="gsw"} option is the best scheme, because
    it uses derivatives of density computed with \emph{local} reference
    pressure.  
    
    For precise work, it makes sense to skip \code{swN2} entirely, choosing
    whether, what, and how to smooth based on an understanding of fundamental
    principles as well as data practicalities.
    
} 

\value{Square of buoyancy frequency [\eqn{radian^2/s^2}{radian^2/s^2}].}

\examples{
library(oce)
data(ctd)
# Illustrate difference between UNESCO and GSW
p <- pressure(ctd)
ylim <- rev(range(p))
par(mfrow=c(1,3), mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
plot(ctd[["sigmaTheta"]], p, ylim=ylim, type='l', xlab=expression(sigma[theta]))
N2u <- swN2(ctd, eos="unesco")
N2g <- swN2(ctd, eos="gsw")
plot(N2u, p, ylim=ylim, xlab="N2 Unesco", ylab="p", type="l")
d <- 100 * (N2u - N2g) / N2g
plot(d, p, ylim=ylim, xlab="N2 UNESCO-GSW diff. [\%]", ylab="p", type="l")
abline(v=0)
}

\author{Dan Kelley}

\keyword{misc}
