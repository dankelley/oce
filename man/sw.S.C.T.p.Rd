\name{sw.S.C.T.p}

\alias{sw.S.C.T.p}

\title{Seawater salinity from conductivity ratio, temperature and pressure}

\description{Compute salinity, given conductivity ratio, temperature, and pressure.}

\usage{S <- sw.S.C.T.p(C, t, p)}

\arguments{
  \item{C}{conductivity ratio [unitless]}
  \item{t}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{p}{pressure [dbar]}
}

\details{Calculate S from what is actually measured by the CTD,
  \emph{i.e.} the conductivity ratio C, the \emph{in-situ} temperature
  T, and the pressure p.  Often this is done by the CTD processing
  software, but sometimes it is helpful to do this directly, \emph{e.g.}
  when there is a concern about mismatches in sensor response times.  S
  is calculated using the UNESCO algorithm described by Fofonoff and
  Millard (1983).}

\value{Salinity [PSU].}

\seealso{\code{\link{sw.conductivity}}}

\examples{
print(sw.S.C.T.p(1,15,2000))   # 34.25045
print(sw.S.C.T.p(1.2,20,2000)) # 37.24563
print(sw.S.C.T.p(0.65,5,1500)) # 27.99535
}

\references{Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for
 computation of fundamental properties of seawater. \emph{Unesco
 Technical Papers in Marine Science}, \bold{44}, 53 pp}

\author{Dan Kelley}

\keyword{misc}
