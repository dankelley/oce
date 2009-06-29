\name{adp.beam2earth}

\alias{adp.beam2earth}

\title{Convert ADP from beam coordinates to earth coordinates}

\description{Convert ADP velocity components from a beam-based
  coordinate system to an earth-based coordinate system.}

\usage{adp.beam2earth(x)}

\arguments{\item{x}{an object of class \code{"adp"}.}}

\details{This is a direct step for Nortek instruments, and is equivalent
  to \code{\link{adp.frame2earth}(\link{adp.beam2frame}(x))} for RDI
  instruments.}

\value{An object with the first 3 velocitiy indices having been
  altered to represent velocity components in frame (or instrument)
  coordinates.  (For \code{rdi} data, the values at the 4th velocity 
  index are changed to represent the "error" velocity.)

  To indicate the change, the value of \code{metadata$oce.orientation}
  is changed from \code{beam} to \code{frame}.}

\examples{
library(oce)
data(adp)
f <- adp.beam2earth(adp)
summary(f)
plot(f, which=1:3)
}

\references{R D Instruments, 1998.
\emph{ADP Coordinate Transformation, formulas and calculations.}
P/N 951-6079-00 (July 1998).}

\seealso{See \code{\link{read.adp}} for other functions that relate to objects
of class \code{"adp"}.}

\author{Dan Kelley}

\keyword{misc}
