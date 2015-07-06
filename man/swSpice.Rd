% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{swSpice}

\alias{swSpice}

\title{Seawater spiciness}

\description{Compute seawater "spice" (a variable orthogonal to density in TS
  space).}

\usage{swSpice(salinity, temperature=NULL, pressure=NULL)}

\arguments{

  \item{salinity}{either salinity [PSU] (in which case \code{temperature} and
    \code{pressure} must be provided) \strong{or} a \code{ctd} object (in which
    case \code{salinity}, \code{temperature} and \code{pressure} are determined
    from the object, and must not be provided in the argument list).}

  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C] on the
    ITS-90 scale; see \dQuote{Temperature units} in the documentation for
      \code{\link{swRho}}.}

  \item{pressure}{seawater pressure [dbar]}
}

\details{If the first argument is a \code{ctd} object, then salinity,
  temperature and pressure values are extracted from it, and used for the
  calculation.

  Roughly speaking, seawater with a high spiciness is relatively warm and salty
  compared with less spicy water. Another interpretation is that spice is a
  variable measuring distance orthogonal to isopycnal lines on TS diagrams (if
  the diagrams are scaled to make the isopycnals run at 45 degres). The
  definition used here is that of Pierre Flament. (Other formulations exist.)
  Note that pressure is ignored in the definition. Spiceness is sometimes
  denoted \eqn{\pi(S,t,p)}{pi(S,t,p)}.

}


\value{Spice [kg/m\eqn{^3}{^3}].}


\references{P. Flament, 2002.
A state variable for characterizing water masses and their diffusive
stability: spiciness.   \emph{Progr. Oceanog.}, 
\bold{54}, 493-501.
}

\author{Dan Kelley}

\keyword{misc}
