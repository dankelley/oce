\name{adp.beam2xyz}

\alias{adp.beam2xyz}

\title{Convert ADP from beam coordinates to xyz coordinates}

\description{Convert ADP velocity components from a beam-based
  coordinate system to a xyz-based coordinate system.}

\usage{adp.beam2xyz(x, debug=getOption("oce.debug"))}

\arguments{
  \item{x}{an object of class \code{"adp"}.}
  \item{debug}{a debugging flag, 0 for no debugging, and higher values for more and more debugging.}
}

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
  altered to represent velocity components in xyz (or instrument)
  coordinates.  (For \code{rdi} data, the values at the 4th velocity 
  index are changed to represent the "error" velocity.)

  To indicate the change, the value of \code{metadata$oce.orientation}
  is changed from \code{beam} to \code{xyz}.}

\references{

1. R D Instruments, 1998.
\emph{ADP Coordinate Transformation, formulas and calculations.}
P/N 951-6079-00 (July 1998).

2. WHOI/USGS-provided Matlab code for beam-enu transformation
\url{http://woodshole.er.usgs.gov/pubs/of2005-1429/MFILES/AQDPTOOLS/beam2enu.m}

}

\seealso{See \code{\link{read.adp}} for other functions that relate to objects
of class \code{"adp"}.}

\author{Dan Kelley}

\keyword{misc}
