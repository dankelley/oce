\name{swSCTp}

\alias{swSCTp}

\title{Seawater salinity from conductivity, temperature and pressure}

\description{Compute salinity, given electrical conductivity, temperature, and pressure.}

\usage{swSCTp(conductivity, temperature, pressure, conductivityUnit=c("ratio", "mS/cm", "S/m"))}

\arguments{
  \item{conductivity}{conductivity ratio [unitless]}
  \item{temperature}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{pressure}{pressure [dbar]}
  \item{conductivityUnit}{string indicating the unit used for conductivity;
      \code{ratio} means actual conductivity divided by 4.2914 S/m.}
}


\details{Calculate salinity from what is actually measured by the CTD,
    \emph{i.e.} conductivity, the \emph{in-situ} temperature and pressure.
    Often this is done by the CTD processing software, but sometimes it is
    helpful to do this directly, \emph{e.g.}  when there is a concern about
    mismatches in sensor response times.  Salinity is calculated using the
    UNESCO algorithm described by Fofonoff and Millard (1983).}

\value{Practical salinity [PSU].}

\seealso{For thermal (as opposed to electrical) conductivity, see \code{\link{swConductivity}}.}

\examples{
print(swSCTp(1,15,2000))   # 34.25045
print(swSCTp(1.2,20,2000)) # 37.24563
print(swSCTp(0.65,5,1500)) # 27.99535
}

\references{Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for
 computation of fundamental properties of seawater. \emph{Unesco
 Technical Papers in Marine Science}, \bold{44}, 53 pp}

\author{Dan Kelley}

\keyword{misc}
