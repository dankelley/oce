\name{adcp.earth2other}

\alias{adcp.earth2other}

\title{Rotate ADCP currents from earth coordinates to other coordinates}

\description{Convert ADCP velocity components from an earth-based
  coordinate system to another system, perhaps to align axes with the
  coastline.}

\usage{adcp.earth2other(x, heading=0, pitch=0, roll=0)}

\arguments{
  \item{x}{an object of class \code{"adcp"}.}
  \item{heading}{ angle, in degrees, to be added to the
    heading.  See \dQuote{Details}.}
  \item{pitch}{angle to be added to the pitch.}
  \item{roll}{angle to be added to the roll.}
}

\details{The supplied angles specify rotations to be made around the
  axes for heading, pitch, and roll.  Think of this as addition.  For
  example, an object \code{d} storing current of speed \code{U} running 
  from southwest to northeast has equal and positive eastward and 
  northward components in earth coordinates, so setting \code{heading=45} 
  yields horizontal components \code{d$data$ma$v[,1,]=U} and 
  \code{d$data$ma$v[,2,]=0}, leaving vertical component
  \code{d$data$ma$v[,3,]} unaltered.

  Note that the stored values for heading, pitch, and roll are
  \emph{not} altered.  These values refer the instrument orientation,
  not to the values of the corresponding arguments supplied to the
  present function.

  The code is similar to that used by \code{\link{adcp.frame2earth}},
  but in the present case the signs of \code{pitch} and \code{roll} are
  obeyed, regardless of whether the instrument points up or down,
  because earth coordinates already account for instrument alignment.}

\value{An object with the \code{data$ma$v[,1:3,]} altered appropriately,
 and with \code{metadata$oce.orientation} changed from \code{earth} to
  \code{other}.}

\examples{
library(oce)
data(adcp)
f <- adcp.beam2frame(adcp)
e <- adcp.frame2earth(f)
plot(e, which=1:3)
o <- adcp.earth2other(e, heading=31.5)
plot(o, which=1:3)
}

\references{RD Instruments, 1998.
\emph{ADCP Coordinate Transformation, formulas and calculations.}
P/N 951-6079-00 (July 1998)}

\seealso{\code{\link{read.adcp}} reads \code{adcp} objects,
\code{\link{summary.adcp}} summarizes them, and \code{\link{plot.adcp}} plots them.
\code{\link{adcp}} provides a sample ADCP dataset.
\code{\link{adcp.beam.attenuate}} converts an \code{adcp} object from beam to velocity.
\code{\link{adcp.beam2frame}} converts an \code{adcp} object from beam coordinates to 
frame coordinates.
\code{\link{adcp.frame2earth}} converts an \code{adcp} object from frame coordinates 
to earth coordinates.
}

\author{Dan Kelley}

\keyword{misc}
