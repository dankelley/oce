\name{tdrPatm}
\alias{tdrPatm}
\title{Estimate atmospheric pressure in tdr record}
\description{Estimate atmospheric pressure in tdr record}
\usage{tdrPatm(x, dp=0.5)}
\arguments{
  \item{x}{A \code{tdr} object, or a list of pressures (in decibars).}
  \item{dp}{Half-width of pressure window to be examined (in decibars).}
}

\details{Pressures must be in decibars for this to work.  First, a
  subset of pressures is created, in which the range is \code{sap-dp} to
  \code{sap+dp}.  Here, \code{sap}=10.1325 dbar is standard sealevel
  atmospheric pressure.  Within this window, three measures of central
  tendency are calculated: the median, the mean, and a weighted mean
  that has weight given by \eqn{exp(-2*((p - sap) / dp)^2)}{exp(-2*((p -
    sap) / dp)^2)}.}

\value{A list of four estimates: \code{sap}, the median, the mean, and
  the weighted mean.}

\seealso{The documentation for \code{\link{tdr-class}} explains the structure of
    PT objects, and also outlines the other functions dealing with them.}

\examples{
library(oce)
data(tdr)
print(tdrPatm(tdr))
}

\author{Dan Kelley}
\keyword{misc}
