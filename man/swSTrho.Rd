\name{swSTrho}

\alias{swSTrho}

\title{Seawater salinity from temperature and density}

\description{Compute Practical or Absolute Salinity, given in-situ or
    Conservative Temperature, density, and pressure. This is mainly used to
    draw isopycnal lines on TS diagrams, hence the dual meanings for salinity
    and temperature, depending on the value of \code{eos}.}

\usage{swSTrho(temperature, density, pressure, eos=getOption("oceEOS", default="gsw"))}

\arguments{

  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C], if
      \code{eos="unesco"}, or Conservative Temperature, if \code{eos="gsw"}.}
  \item{density}{\emph{in-situ} density or sigma value [\eqn{kg/m^3}{kg/m^3}]}
  \item{pressure}{\emph{in-situ} pressure [dbar]}
  \item{eos}{equation of state, either \code{"unesco"} [1,2] or \code{"gsw"}
      [3,4].}
}

\details{For \code{eos="unesco"}, finds the practical salinity that yields the
    given density, with the given in-situ temperature and pressure.  The method
    is a bisection search with a salinity tolerance of 0.001.  For
    \code{eos="gsw"}, the function \code{\link[gsw]{gsw_SA_from_rho}} in the
    \CRANpkg{gsw} packae is used to infer Absoute Salinity from Conservative
    Temperature.}

\value{Practical Salinity, if \code{eos="unesco"}, or Absolute Salinity, if
    \code{eos="gsw"}.}

\seealso{\code{\link{swTSrho}}}

\examples{
swSTrho(10, 22, 0) # 28.76285
swSTrho(10, 22, 0, eos="gsw") # 28.76285
swSTrho(10, 22, 0, eos="unesco") # 28.65115
}

\references{

    [1] Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for computation of
    fundamental properties of seawater. \emph{Unesco Technical Papers in Marine
        Science}, \bold{44}, 53 pp

    [2] Gill, A.E., 1982. \emph{Atmosphere-ocean Dynamics}, Academic Press, New
    York, 662 pp.

    [3] IOC, SCOR, and IAPSO (2010). The international thermodynamic equation
    of seawater-2010: Calculation and use of thermodynamic properties.
    Technical Report 56, Intergovernmental Oceanographic Commission, Manuals
    and Guide.

    [4] McDougall, T.J. and P.M. Barker, 2011: Getting started with TEOS-10 and
    the Gibbs Seawater (GSW) Oceanographic Toolbox, 28pp., SCOR/IAPSO WG127,
    ISBN 978-0-646-55621-5.

}

\author{Dan Kelley}

\keyword{misc}
