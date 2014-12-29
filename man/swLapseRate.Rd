\name{swLapseRate}

\alias{swLapseRate}

\title{Seawater lapse rate}

\description{Compute adiabatic lapse rate}

\usage{swLapseRate(salinity, temperature=NULL, pressure=NULL,
    longitude, latitude, eos=getOption("oceEOS", default="gsw"))}

\arguments{
  \item{salinity}{either salinity [PSU] (in which case
    \code{temperature} and \code{pressure} must be provided) \strong{or}
    a \code{ctd} object (in which case \code{salinity},
    \code{temperature} and \code{pressure} are determined from the
    object, and must not be provided in the argument list).}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{pressure}{pressure [dbar]}
  \item{longitude}{longitude of observation (only used if \code{eos="gsw"}; see
      \sQuote{Details}).}
  \item{latitude}{latitude of observation (only used if \code{eos="gsw"}; see
      \sQuote{Details}).}
  \item{eos}{equation of state, either \code{"unesco"} [1,2] or \code{"gsw"}
      [3,4].}

}

\details{If the first argument is a \code{ctd} object, then salinity, etc. are
    extracted from it, and used for the calculation.

    If \code{eos="unesco"}, the density is calculated using the UNESCO equation
    of state for seawater [1,2], and if \code{eos="gsw"}, the GSW formulation
    [3,4] is used.

    In the \code{gsw} case, if \code{longitude} and \code{latitude} are
    missing, default values of 300E and 30N will be used, and a warning will be
    generated if \code{getOption("oceEOSwarning")} is \code{TRUE}.

}

\value{Lapse rate [\eqn{deg}{deg}C/m].}

\examples{
 lr <- swLapseRate(40, 40, 10000) # 3.255976e-4
}

\references{Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for
 computation of fundamental properties of seawater. \emph{Unesco
 Technical Papers in Marine Science}, \bold{44}, 53 pp.  (Section 7,
 pages 38-40) }
\author{Dan Kelley}

\keyword{misc}
