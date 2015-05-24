\name{swDepth}

\alias{swDepth}
\alias{swZ}

\title{Water depth}

\description{Compute depth from pressure and latitude.}

\usage{
swDepth(pressure, latitude=45, eos=getOption("oceEOS", default="gsw"))
swZ(pressure, latitude=45, eos=getOption("oceEOS", default="gsw"))
}

\arguments{
  \item{pressure}{either pressure [dbar], in which case \code{lat} must also be
    given, or a \code{ctd} object, in which case \code{lat} will be
    inferred from the object.}
  \item{latitude}{Latitude in \eqn{^\circ}{deg}N or radians north of the equator.}
  \item{eos}{indication of formulation to be used, either \code{"unesco"} or
      \code{"gsw"}.}
}

\details{If \code{eos="unesco"} then depth is calculated from pressure using
    Saunders and Fofonoff's method, with the formula refitted for 1980 UNESCO
    equation of state [1].  If \code{eos="gsw"}, then
    \code{\link[gsw]{gsw_z_from_p}} from the \CRANpkg{gsw} package [2,3] is used.}

\value{For \code{swDepth}, depth below the ocean surface, in metres.
  For \code{swZ}, the negative of the depth, i.e. the distance above
  the surface.}

\examples{d <- swDepth(10, 45)}

\references{[1] Unesco 1983. Algorithms for computation of fundamental
      properties of seawater, 1983. \emph{Unesco Tech. Pap. in
      Mar. Sci.}, No. 44, 53 pp.

    [2] IOC, SCOR, and IAPSO (2010). The international thermodynamic equation
    of seawater-2010: Calculation and use of thermodynamic properties.
    Technical Report 56, Intergovernmental Oceanographic Commission, Manuals
    and Guide.

    [3] McDougall, T.J. and P.M. Barker, 2011: Getting started with TEOS-10 and
    the Gibbs Seawater (GSW) Oceanographic Toolbox, 28pp., SCOR/IAPSO WG127,
    ISBN 978-0-646-55621-5.
}


\author{Dan Kelley}

\keyword{misc}
