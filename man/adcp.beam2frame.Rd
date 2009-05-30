\name{adcp.beam2frame}

\alias{adcp.beam2frame}

\title{Convert ADCP from beam coordinates to frame coordinates}

\description{Convert ADCP velocity components from a beam-based
  coordinate system to a frame-based coordinate system.}

\usage{adcp.beam2frame(x)}

\arguments{\item{x}{an object of class \code{"adcp"}.}}

\details{The action depends on the type of object.

  For a 3-beam \code{aquadopp} object, the beams are transformed
  into velocities using the matrix stored in the header.

  For 4-beam \code{rdi} object, the beams are converted to velocity
  components using formulae from section 5.5 of \emph{RD Instruments}
  (1998), viz. the along-beam velocity components
  \eqn{B_1}{B1},
  \eqn{B_2}{B2},
  \eqn{B_3}{B3},
  and
  \eqn{B_4}{B4}
  are used to calculate velocity components in a cartesian
  system referenced to the instrument using the following formulae:
  \eqn{u=ca(B_1-B_2)}{u=c*a*(B1-B2)},
  \eqn{v=ca(B_4-B_3)}{v=c*a*(B4-B3)},
  \eqn{w=-b(B_1+B_2+B_3+B_4)}{w=-b*(B1+B2+B3+B4)},
  and an estimate of the error in velocity is calculated using
  \eqn{e=d(B_1+B_2-B_3-B_4)}{e=d*(B1 + B2 - B3 - B4)}

  (Note that the multiplier on \eqn{e}{e} is subject to discussion;
  RDI suggests one multiplier, but some oceanographers favour another.)

  In the above, 
  \eqn{c=1}{c=1} if the beam geometry is convex, and
  \eqn{c=-1}{c=-1} if the beam geometry is concave, 
  \eqn{a=1/(2\sin\theta)}{a=1/(2*sin(theta))},
  \eqn{b=1/(4\cos\theta)}{b=1/(4*cos(theta))}
  and
  \eqn{d=a/\sqrt{2}}{d=a/sqrt(2)}, where \eqn{\theta}{theta}
  is the angle the beams make to the instrument \dQuote{vertical}.}

\value{An object with the first 3 velocitiy indices having been
  altered to represent velocity components in frame (or instrument)
  coordinates.  (For \code{rdi} data, the values at the 4th velocity 
  index are changed to represent the "error" velocity.)

  To indicate the change, the value of \code{metadata$oce.orientation}
  is changed from \code{beam} to \code{frame}.}

\examples{
library(oce)
data(adcp)
f <- adcp.beam2frame(adcp)
summary(f)
plot(f, which=1:3)
}

\references{R D Instruments, 1998.
\emph{ADCP Coordinate Transformation, formulas and calculations.}
P/N 951-6079-00 (July 1998).}

\seealso{\code{\link{read.adcp}} reads \code{adcp} objects,
\code{\link{summary.adcp}} summarizes them, and \code{\link{plot.adcp}} plots them.
\code{\link{adcp}} provides a sample ADCP dataset.
\code{\link{adcp.beam.attenuate}} converts an \code{adcp} object from beam to velocity.
\code{\link{adcp.earth2other}} rotates \code{adcp} object currents from earth coordinates 
to other coordinates.
\code{\link{adcp.frame2earth}} converts an \code{adcp} object from frame coordinates to 
earth coordinates.}

\author{Dan Kelley}

\keyword{misc}
