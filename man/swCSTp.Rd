% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{swCSTp}

\alias{swCSTp}

\title{Electrical conductivity ratio from salinity, temperature and pressure}

\description{Compute electrical conductivity ratio based on salinity,
  temperature, and pressure (relative to the conductivity of seawater
  with salinity=35, temperature68=15, and pressure=0).}

\usage{swCSTp(salinity, temperature, pressure, 
                   eos=getOption("oceEOS", default="gsw"))}

\arguments{
  \item{salinity}{practical salinity, or a CTD object (in which case its
    temperature and pressure are used, and the next two arguments are ignored)}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C], defined on
    the ITS-90 scale; see \dQuote{Temperature units} in the documentation for
    \code{\link{swRho}}.}
  \item{pressure}{pressure [dbar]}
  \item{eos}{equation of state, either \code{"unesco"} or \code{"gsw"}.}
}

\details{If \code{eos="unesco"}, the calculation is done by a bisection
  root search on the UNESCO formula relating salinity to conductivity,
  temperature, and pressure (see \code{\link{swSCTp}}).  If it is
  \code{"gsw"} then the Gibbs-SeaWater formulation is used, via
  \code{\link{gsw_C_from_SP}}.}

\value{Conductivity ratio [unitless], i.e. the ratio of conductivity to the
  conductivity at salinity=35, temperature=15 (IPTS-68 scale) and pressure=0, which has
  numerical value 42.9140 (see Culkin and Smith, 1980).}

\seealso{For thermal (as opposed to electrical) conductivity, see
  \code{\link{swThermalConductivity}}. For computation of salinity from
  electrical conductivity, see \code{\link{swSCTp}}.}

\examples{
swCSTp(34.25045, T90fromT68(15), 2000, eos="unesco") # 1
swCSTp(34.25045, 15, 2000, eos="unesco") # 1.000081
swCSTp(34.25045, 15, 2000, eos="gsw")  # 0.9999982
}

\references{Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for computation
  of fundamental properties of seawater. \emph{Unesco Technical Papers in Marine
    Science}, \bold{44}, 53 pp

  Culkin, F., and Norman D. Smith, 1980. Determination of the
  concentration of potassium chloride solution having the same
  electrical conductivity, at 15 C and infinite frequency, as standard
  seawater of salinity 35.0000 ppt (Chlorinity 19.37394 ppt). \emph{IEEE Journal
  of Oceanic Engineering}, \bold{5}, pp 22-23.}

\author{Dan Kelley}

\keyword{misc}
