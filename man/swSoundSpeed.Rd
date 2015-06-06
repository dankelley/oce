\name{swSoundSpeed}

\alias{swSoundSpeed}

\title{Seawater sound speed}

\description{Compute the seawater speed of sound.}

\usage{swSoundSpeed(salinity, temperature=NULL, pressure=NULL,
    longitude=300, latitude=30, eos=getOption("oceEOS", default="gsw"))}

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

\details{If the first argument is a \code{ctd} or \code{section} object, then
    values for salinity, etc., are extracted from it, and used for the
    calculation, and the corresponding arguments to the present function are
    ignored.

    If \code{eos="unesco"}, the sound speed is calculated using the formulation
    in section 9 of Fofonoff and Millard (1983). If \code{eos="gsw"}, then the
    \code{\link[gsw]{gsw_sound_speed}} function from the \CRANpkg{gsw} package
    is used.

}

\value{Sound speed [m/s].}

\examples{
s <- swSoundSpeed(40, 40, 10000) # 1731.995 (p48 of Fofonoff + Millard 1983)
}

\references{Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for
 computation of fundamental properties of seawater. \emph{Unesco
 Technical Papers in Marine Science}, \bold{44}, 53 pp.  (See section
 9.)  }

\author{Dan Kelley}

\keyword{misc}
