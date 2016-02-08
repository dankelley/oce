\name{adv}

\docType{data}

\alias{adv}

\title{ADV (acoustic-doppler velocimeter) dataset}

\description{This is a sampling of measurements made with a Nortek Vector
    acoustic Doppler velocimeter deployed as part of the St Lawrence Internal
    Wave Experiment (SLEIWEX).  Various identifying features have been
    redacted.}

\examples{
\dontrun{
library(oce)
data(adv)

# Velocity time-series
plot(adv)

# Spectrum of upward component of velocity, with ``turbulent'' reference line
s <- spectrum(adv[["v"]][,3],plot=FALSE)
plot(log10(s$freq), log10(s$spec), type='l')
for (a in seq(-20, 20, by=1))
    abline(a=a, b=-5/3, col='gray', lty='dotted')
}
}

\usage{data(adv)}

\source{This file came from the SLEIWEX-2008 experiment.}

\seealso{The documentation for \code{\link{adv-class}} explains the structure
    of \code{adv} objects, and also outlines the other functions dealing with
    them.}

\author{Dan Kelley}

\keyword{datasets}

