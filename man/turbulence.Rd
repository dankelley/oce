\name{turbulence}

\docType{data}

\alias{turbulence}

\title{Grant et al. (1962) turbulence data}

\description{Turbulence spectrum phi, as a function of wavenumber k (cgs units,
    both) for a run on 0905/3/10/59, with data in their Appendix 1 and graph in
    their Figure 9 and Appendix 1.}

\usage{data(turbulence)}

\examples{
library(oce)
data(turbulence)
plot(log10(turbulence$k), log10(turbulence$phi),
     xlab=expression(log[10]*k), ylab=expression(log[10]*phi), pch=20,
     xlim=c(-2,1.5), ylim=c(-6,4))
grid()
power <- -5/3
intercept <- -0.40
abline(intercept, power)
}

\references{H. L. Grant and R. W. Stewart and A. Moilliet, 1962.  Turbulence
    spectra from a tidal channel.  Journal of Fluid Mechanics, 12(2): 241-268. }

\keyword{datasets}
