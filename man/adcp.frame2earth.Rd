\name{adcp.frame2earth}

\alias{adcp.frame2earth}

\title{Convert ADCP from frame coordinates to earth coordinates}

\description{Convert ADCP velocity components from a frame-based
  coordinate system to an earth-based coordinate system, by using
  the instrument's recording of heading,  pitch, and roll.}

\usage{adcp.frame2earth(x)}

\arguments{\item{x}{an object of class \code{"adcp"}.}}

\details{Beams are rotated using formulae from page 13 and 14 of
  \emph{RD Instruments} (1998).  Note that for downward-facing
  instruments (i.e. for \code{x$metadata$orientation="down"}), pitch and
  roll are multiplied by -1 before the calculation is done.}

\value{An object with the \code{data$ma$v[,1:3,]} altered appropriately,
  and with \code{metadata$oce.orientation} changed from \code{frame} to
  \code{earth}.}

\examples{
library(oce)
data(adcp)
f <- adcp.beam2frame(adcp)
e <- adcp.frame2earth(f)
plot(e, which=1:3)
}

\references{RD Instruments, 1998.
\emph{ADCP Coordinate Transformation, formulas and calculations.}
P/N 951-6079-00 (July 1998).}

\seealso{\code{\link{read.adcp}} reads \code{adcp} objects,
\code{\link{summary.adcp}} summarizes them, and \code{\link{plot.adcp}} plots them.
\code{\link{adcp}} provides a sample ADCP dataset.
\code{\link{adcp.beam.attenuate}} converts an \code{adcp} object from beam to velocity.
\code{\link{adcp.beam2frame}} converts an \code{adcp} object from beam coordinates to 
frame coordinates.
\code{\link{adcp.earth2other}} rotates \code{adcp} object currents from earth coordinates 
to other coordinates.}

\author{Dan Kelley}

\keyword{misc}
