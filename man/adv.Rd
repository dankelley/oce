\name{adv}

\docType{data}

\alias{adv}

\title{ADV (acoustic-doppler velocimeter) dataset}

\description{This is a sampling of measurements made with a Nortek Vector
    acoustic Doppler velocimeter deployed as part of the St Lawrence Internal
    Wave Experiment (SLEIWEX).}

\examples{
library(oce)
data(adv)

# Velocity time-series
plot(adv)

# Does the spectrum of upward velocity look turbulent?
s <- spectrum(adv$data$v[,3],plot=FALSE)
plot(log10(s$freq), log10(s$spec),type='l')
for (a in seq(-20, 20, by=1))
    abline(a=a, b=-5/3, col='gray', lty='dotted')
}

\usage{data(adv)}

\source{This file came from the SLEIWEX-2008 experiment.}

\seealso{See \code{\link{read.adv}} for notes on the contents of \code{adv}
    objects, and for functions to work with them.  Also, see
    \code{\link{plot.adv}} to learn about plotting ADP objects.}

\author{Dan Kelley}

\keyword{datasets}
