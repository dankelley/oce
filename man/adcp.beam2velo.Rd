\name{adcp.beam2velo}

\alias{adcp.beam2velo}

\title{Convert ADCP from beam to velocity}

\description{Convert ADCP from beam to velocity}

\usage{adcp.beam2velo(x)}

\arguments{
  \item{x}{an object of class \code{"adcp"}}
}

\details{Beams are converted to velocity components using formulae from
  section 5.5 of a Teledyne-RDI manual.

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
\eqn{c=-1}{c=01} if the beam geometry is concave, 

\eqn{a=1/(2\sin(\theta))}{a=1/(2*sin(theta))}

\eqn{b=1/(4\cos(\theta))}{b=1/(4*cos(theta))}

and

\eqn{d=a/\sqrt{2}}{d=a/sqrt(2)}

}

\value{An object of \code{\link[base]{class}} \code{"adcp"}.}

\examples{
\dontrun{
library(oce)
d <- read.oce("~/SL08F001.000",read=60)
e <- adcp.beam2velo(d)
summary(e)
plot(e)
}
}

\references{RDI Instruments, 1998.
  ADCP Coordinate Transformation, formulas and calculations.
  P/N 951-6079-00 (July 1998).
}


\author{Dan Kelley}

\keyword{misc}
