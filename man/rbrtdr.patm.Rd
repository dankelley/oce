\name{rbrtdr.patm}
\alias{rbrtdr.patm}
\title{Estimate atmospheric pressure in RBR record}
\description{Estimate atmospheric pressure in RBR record}
\usage{rbrtdr.patm(x, dp=0.5)}
\arguments{
  \item{x}{A \code{rbrtdr} object, e.g. as read by
    \code{\link{read.rbrtdr}}, or a list of pressure (in decibars).}
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

\seealso{See \code{\link{read.rbrtdr}} for more information on dealing
  with the \code{rbrtdr} data type, including examples.}

\author{Dan Kelley}
\keyword{misc}
