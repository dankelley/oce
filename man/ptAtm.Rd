\name{ptPatm}
\alias{ptPatm}
\title{Estimate atmospheric pressure in pt record}
\description{Estimate atmospheric pressure in pt record}
\usage{ptPatm(x, dp=0.5)}
\arguments{
  \item{x}{A \code{pt} object, or a list of pressures (in decibars).}
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

\seealso{See \code{\link{read.pt}} for more information on dealing
  with the \code{pt} data type, including examples.}

\examples{
library(oce)
data(ptr)
print(ptPatm(ptr))
}

\author{Dan Kelley}
\keyword{misc}
