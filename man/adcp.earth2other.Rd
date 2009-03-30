\name{adcp.earth2other}

\alias{adcp.earth2other}

\title{Rotate ADCP currents from earth coordinates to other coordinates}

\description{Convert ADCP velocity components from an earth-based
  coordinate system to another system, perhapsto align axes with the
  coastline.}

\usage{adcp.earth2other(x, heading=0, pitch=0, roll=0)}

\arguments{
  \item{x}{an object of class \code{"adcp"}}
  \item{heading}{numeric, the angle, in degrees, to be added to the
  heading.  See \dQuote{Details}.}
  \item{pitch}{as \code{heading}, but for pitch.}
  \item{roll}{as \code{heading}, but for roll.}

}

\details{The supplied angles specify rotations to be made around the
  axes for heading, pitch, and roll.  Think of this as addition.  For
  example, a current of speed \code{U} running from southwest to
  northeast has equal and positive eastward and northward components in
  earth coordinates, so setting \code{heading=45} yields \code{u'=U} and
  \code{v'=0}.  Note that the stored values for heading, pitch, and roll
  are \emph{not} adjusted; these values indicate the instrument
  orientation and so it makes no sense to alter them after the fact.

  The code is similar to that used by \code{\link{adcp.frame2earth}},
  but in the present case the signs of \code{pitch} and \code{roll} are
  obeyed, regardless of whether the instrument points up or down,
  because earth coordinates already account for instrument alignment.}

\value{An object of \code{\link[base]{class}} \code{"adcp"}, with
  columns renamed from \code{east}, \code{north} and \code{up} to \code{u'},
  \code{v'}, and \code{w'}.  (Note that \code{e} is left unaltered.)
  Also, \code{metadata$oce.orientation} is changed from \code{earth} to
  \code{other}.}

\examples{
library(oce)
data(adcp)
f <- adcp.beam2frame(adcp)
e <- adcp.frame2earth(f)
plot(e, which=1:3)
o <- adcp.earth2other(e, heading=40)
plot(o, which=1:3)
}

\references{
RD Instruments, 1998.
\emph{ADCP Coordinate Transformation, formulas and calculations.}
P/N 951-6079-00 (July 1998)
}

\author{Dan Kelley}

\keyword{misc}
