\name{adp.enu2other}

\alias{adp.enu2other}

\title{Convert ADP from east-north-up coordinates to other coordinates}

\description{Convert ADP velocity components from an enu-based
  coordinate system to another system, perhaps to align axes with the
  coastline.}

\usage{adp.enu2other(x, heading=0, pitch=0, roll=0)}

\arguments{
  \item{x}{an object of class \code{"adp"}.}
  \item{heading}{ angle, in degrees, to be added to the
    heading.  See \dQuote{Details}.}
  \item{pitch}{angle to be added to the pitch.}
  \item{roll}{angle to be added to the roll.}
}

\details{The supplied angles specify rotations to be made around the
  axes for heading, pitch, and roll.  Think of this as addition.  For
  example, an object \code{d} storing current of speed \code{U} running 
  from southwest to northeast has equal and positive eastward and 
  northward components in enu coordinates, so setting \code{heading=45} 
  yields horizontal components \code{d$data$ma$v[,1,]=U} and 
  \code{d$data$ma$v[,2,]=0}, leaving vertical component
  \code{d$data$ma$v[,3,]} unaltered.

  Note that the stored values for heading, pitch, and roll are
  \emph{not} altered.  These values refer the instrument orientation,
  not to the values of the corresponding arguments supplied to the
  present function.

  The code is similar to that used by \code{\link{adp.xyz2enu}},
  but in the present case the signs of \code{pitch} and \code{roll} are
  obeyed, regardless of whether the instrument points up or down,
  because enu coordinates already account for instrument alignment.}

\value{An object with code{data$ma$v[,1:3,]} altered appropriately,
 and \code{metadata$oce.coordinate} changed from \code{enu} to
  \code{other}.}

\examples{
library(oce)
data(adp)
o <- adp.enu2other(adp, heading=31.5)
plot(o, which=1:3)
}

\references{RD Instruments, 1998.
\emph{ADP Coordinate Transformation, formulas and calculations.}
P/N 951-6079-00 (July 1998)}

\seealso{See \code{\link{read.adp}} for other functions that relate to objects
of class \code{"adp"}.}

\author{Dan Kelley}

\keyword{misc}
