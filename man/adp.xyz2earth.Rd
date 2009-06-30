\name{adp.frame2earth}

\alias{adp.frame2earth}

\title{Convert ADP from frame coordinates to earth coordinates}

\description{Convert ADP velocity components from a frame-based
  coordinate system to an earth-based coordinate system, by using
  the instrument's recording of heading,  pitch, and roll.}

\usage{adp.frame2earth(x)}

\arguments{\item{x}{an object of class \code{"adp"}.}}

\details{Different formulae are used for different instruments, e.g for
  RDI instruments, beams are rotated using formulae from page 13 and 14
  of \emph{RD Instruments} (1998).}

\value{An object with the \code{data$ma$v[,1:3,]} altered appropriately,
  and with \code{metadata$oce.orientation} changed from \code{frame} to
  \code{earth}.}

\examples{
library(oce)
data(adp)
f <- adp.beam2frame(adp)
e <- adp.frame2earth(f)
plot(e, which=1:3)
}

\references{RD Instruments, 1998.
\emph{ADCP Coordinate Transformation, formulas and calculations.}
P/N 951-6079-00 (July 1998).}

\seealso{See \code{\link{read.adp}} for other functions that relate to objects
of class \code{"adp"}.}

\author{Dan Kelley}

\keyword{misc}
