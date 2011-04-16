\name{enuToOtherAdp}

\alias{enuToOtherAdp}

\title{Convert ADP from east-north-up coordinates to other coordinates}

\description{Convert ADP velocity components from an enu-based
  coordinate system to another system, perhaps to align axes with the
  coastline.}

\usage{enuToOtherAdp(x, heading=0, pitch=0, roll=0)}

\arguments{
  \item{x}{an object of class \code{"adp"}.}
  \item{heading}{number or vector of numbers, giving the angle, in degrees, to
      be added to the heading.  See \dQuote{Details}.}
  \item{pitch}{as \code{heading} but for pitch.}
  \item{roll}{as \code{heading} but for roll.}
}

\details{The supplied angles specify rotations to be made around the
  axes for which heading, pitch, and roll are defined.  For example, an
  eastward current will point southeast if \code{heading=45} is used.
  
  The returned value has heading, pitch, and roll matching those of \code{x},
  so these angles retain their meaning as the instrument orientation.

  NOTE: this function works similarly to \code{\link{xyzToEnuAdp}}, except
  that in the present function, it makes no difference whether the instrument
  points up or down, etc.
}

\value{An object with \code{data$ma$v[,1:3,]} altered appropriately,
 and \code{metadata$oce.coordinate} changed from \code{enu} to
  \code{other}.}

\examples{
library(oce)
data(adp)
o <- enuToOtherAdp(adp, heading=-31.5)
plot(o, which=1:3)
}

\references{RD Instruments, 1998.
\emph{ADP Coordinate Transformation, formulas and calculations.}
P/N 951-6079-00 (July 1998)}

\seealso{See \code{\link{read.adp}} for other functions that relate to objects
    of class \code{"adp"}.}

\author{Dan Kelley}

\keyword{misc}
