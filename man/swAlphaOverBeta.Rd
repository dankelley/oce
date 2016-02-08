\name{swAlphaOverBeta}

\alias{swAlphaOverBeta}

\title{Ratio of seawater thermal expansion coefficient to haline contraction coefficient}

\description{Compute \eqn{\alpha/\beta}{alpha/beta} using McDougall's (1987) algorithm.}

\usage{swAlphaOverBeta(salinity, temperature=NULL, pressure=NULL,
       longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))}

\arguments{
  \item{salinity}{either practial salinity (in which case \code{temperature}
      and \code{pressure} must be provided) \strong{or} an \code{oce} object
      (in which case \code{salinity}, etc. are inferred from the object).}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{pressure}{pressure [dbar]}
  \item{longitude}{longitude of observation (only used if \code{eos="gsw"}; see
      \sQuote{Details}).}
  \item{latitude}{latitude of observation (only used if \code{eos="gsw"}; see
      \sQuote{Details}).}
  \item{eos}{equation of state, either \code{"unesco"} or \code{"gsw"}.}
}

\details{If the first argument is a \code{ctd} object, then salinity,
  temperature and pressure values are extracted from it, and used for
  the calculation.}

\value{Value in psu/\eqn{^\circ}{deg}C.}

\examples{
swAlphaOverBeta(40, 10, 4000, eos="unesco") # 0.3476
}

\references{The \code{eos="unesco"} formulae are based on the UNESCO equation
    of state, but are formulaed empirically by Trevor J. McDougall, 1987,
    Neutral Surfaces, Journal of Physical Oceanography, volume 17, pages
    1950-1964. The \code{eos="gsw"} formulae come from GSW; see references in
    the \code{\link{swRho}} documentation.}


\author{Dan Kelley}

\keyword{misc}
