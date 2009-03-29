\name{adcp.beam2frame}

\alias{adcp.beam2frame}

\title{Convert ADCP from beam coordinates to frame coordinates}

\description{Convert ADCP velocity components from a beam-based
  coordinate system (bm1, bm2, bm3, bm4) to a frame-based coordinate
  system (u, v, w, e).}

\usage{adcp.beam2frame(x)}

\arguments{
  \item{x}{an object of class \code{"adcp"}}
}

\details{Beams are converted to velocity components using formulae from
  section 5.5 of \emph{RD Instruments} (1998).

  Velocity components
  \eqn{u}{u},
  \eqn{v}{v}
  and
  \eqn{w}{w},
  along with error estimate
  \eqn{e}{e} 
  are defined in terms of beam values
  \eqn{B_1}{B1},
  \eqn{B_2}{B2},
  \eqn{B_3}{B3},
  and
  \eqn{B_4}{B4}
  as follows.

\eqn{u=ca(B_1-B_2)}{u=c*a*(B1-B2)}

\eqn{v=ca(B_4-B_3)}{v=c*a*(B4-B3)}

\eqn{w=-b(B_1+B_2+B_3+B_4)}{w=-b*(B1+B2+B3+B4)}

\eqn{e=d(B_1+B_2-B_3-B_4)}{e=d*(B1 + B2 - B3 - B4)}

where

\eqn{c=1}{c=1} if the beam geometry is convex, and
\eqn{c=-1}{c=-1} if the beam geometry is concave, 

\eqn{a=1/(2\sin(\theta))}{a=1/(2*sin(theta))}

\eqn{b=1/(4\cos(\theta))}{b=1/(4*cos(theta))}

and

\eqn{d=a/\sqrt{2}}{d=a/sqrt(2)}

}

\value{An object of \code{\link[base]{class}} \code{"adcp"}, with
  columns renamed from \code{bm1}, \code{bm2}, \code{bm3} and \code{bm4}
  to \code{u}, \code{v}, \code{w}, and \code{e}.  Also,
  \code{metadata$oce.orientation} is changed from \code{beam} to
  \code{frame}.}

\examples{
library(oce)
data(adcp)
f <- adcp.beam2frame(adcp)
summary(f)
plot(f, which=1:3)
}

\references{
R D Instruments, 1998.
\emph{ADCP Coordinate Transformation, formulas and calculations.}
P/N 951-6079-00 (July 1998).
}


\author{Dan Kelley}

\keyword{misc}
