\name{swN2}

\alias{swN2}

\title{Squared buoyancy frequency for seawater}

\description{Compute 
\eqn{N^2}{N^2}, the square of the buoyancy frequency for a seawater profile.
}

\usage{swN2(pressure, sigmaTheta=NULL, derivs, \dots)}

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

  \item{\dots}{additional argument, passed to \code{\link{smooth.spline}}, in
      the case that \code{derivs="smoothing"}.  See \dQuote{Details}.}

}
  
\details{If the first argument is a \code{ctd} object, then density is is
    inferred from it, and the \code{sigmaTheta} argument is ignored.

    The core of the method involves differentiating density with respect to
    pressure, and the \code{derivs} argument is used to control how this is
    done, as follows.

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

    Given the derivative, the square of the buoyancy frequency is calculated as
    \eqn{10^{-4}g^2d\sigma_\theta/dp}{1e-4*g^2*d(sigmaTheta)/dp}.

} 

\value{Square of buoyancy frequency [\eqn{radian^2/s^2}{radian^2/s^2}].}

\examples{
library(oce)
data(ctd)
# Illustrate effect of changing df
p <- pressure(ctd)
plot(swN2(ctd), p, ylim=rev(range(p)), xlab="N2",ylab="p", type='l')
lines(swN2(ctd, df=3), p, col="blue")
grid()
}

\author{Dan Kelley}

\keyword{misc}
