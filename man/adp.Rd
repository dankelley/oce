\name{adp}

\docType{data}

\alias{adp}

\title{ADP (acoustic-doppler profiler) dataset}

\description{This is degraded subsample of measurements that were made with an
    upward-pointing ADP manufactured by Teledyne-RDI, as part of the St
    Lawrence Internal Wave Experiment (SLEIWEX).}

\examples{
\dontrun{
library(oce)
data(adp)

# Velocity components.  (Note: we should probably trim some bins at top.)
plot(adp)

# Note that tides have moved the mooring.
plot(adp, which=15:18)
}
}

\usage{data(adp)}

\source{This file came from the SLEIWEX-2008 experiment.}

\seealso{The documentation for \code{\link{adp-class}} explains the structure
    of ADP objects, and outlines other functions that deal with them.}

\author{Dan Kelley}

\keyword{datasets}
