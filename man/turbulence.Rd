\name{turbulence}

\docType{data}

\alias{turbulence}

\title{Grant et al. (1962) turbulence data}

\description{Turbulence spectrum \eqn{phi}{phi}, as a function of wavenumber
    \eqn{k}{k} for a run starring at 0905h on October 3, 1959, with data in
    their Appendix 1 and graph in their Figure 9 and Appendix 1.  The units are
    c.g.s., that is \eqn{k}{k} is in \eqn{cm^{-2}}{cm^{-1}} and \eqn{phi} is in
    \eqn{cm^3/s^2}{cm^3/s^2}.}

\usage{data(turbulence)}

\examples{
library(oce)
## Recreate Fig 9 of Grant et al. (1962), with intercept
## chosen to mimic the published figure
data(turbulence)
plot(log10(turbulence$k), log10(turbulence$phi),
     xlab=expression(log[10]*k), ylab=expression(log[10]*phi), pch=20)
grid()
power <- -5/3
intercept <- -0.3
abline(intercept, power)
}

\references{H. L. Grant and R. W. Stewart and A. Moilliet, 1962.  Turbulence
    spectra from a tidal channel.  Journal of Fluid Mechanics, 12(2): 241-268. }

\keyword{datasets}
