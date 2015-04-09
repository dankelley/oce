\name{swSCTp}

\alias{swSCTp}

\title{Salinity from electrical conductivity, temperature and pressure}

\description{Compute salinity based on electrical conductivity, temperature, and pressure.}

\usage{swSCTp(conductivity, temperature=NULL, pressure=0, conductivityUnit=c("ratio", "mS/cm", "S/m"),
                   eos=getOption("oceEOS", default="gsw"))}

\arguments{
  \item{conductivity}{conductivity ratio, or a CTD object. If a CTD object,
      then the next 3 arguments are ignored, since the information can be
      gleaned from the object.}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{pressure}{pressure [dbar]}
  \item{conductivityUnit}{string indicating the unit used for conductivity;
      \code{ratio} means actual conductivity divided by standard conductivity
      (4.2914 S/m).}
  \item{eos}{equation of state, either \code{"unesco"} or \code{"gsw"}.}
}

\details{Calculate salinity from what is actually measured by a CTD,
    \emph{i.e.} conductivity, \emph{in-situ} temperature and pressure.  Often
    this is done by the CTD processing software, but sometimes it is helpful to
    do this directly, \emph{e.g.}  when there is a concern about mismatches in
    sensor response times.  If \code{eos="unesco"} then salinity is calculated
    using the UNESCO algorithm described by Fofonoff and Millard (1983); if it
    is \code{"gsw"} then the Gibbs-SeaWater formulation is used, via
    \code{\link{gsw_SP_from_C}}.}

\value{Practical salinity.}

\seealso{For thermal (as opposed to electrical) conductivity, see
    \code{\link{swThermalConductivity}}.  For computation of electrical
    conductivity from salinity, see \code{\link{swCSTp}}.}

\examples{
swSCTp(1,15,2000, eos="unesco")                      # 34.25045
swSCTp(1.2,20,2000, eos="unesco")                    # 37.24563
swSCTp(0.65,5,1500, eos="unesco")                    # 27.99535
}

\references{Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for
 computation of fundamental properties of seawater. \emph{Unesco
 Technical Papers in Marine Science}, \bold{44}, 53 pp}

\author{Dan Kelley}

\keyword{misc}
