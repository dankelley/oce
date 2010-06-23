\name{adp.xyz2enu}

\alias{adp.xyz2enu}

\title{Convert ADP from xyz coordinates to enu coordinates}

\description{Convert ADP velocity components from a xyz-based
  coordinate system to an enu-based coordinate system, by using
  the instrument's recording of heading,  pitch, and roll.}

\usage{adp.xyz2enu(x, declination=0, debug=getOption("oce.debug"))}

\arguments{
  \item{x}{an object of class \code{"adp"}.}
  \item{declination}{magnetic declination to be added to the heading, to
    get ENU with N as "true" north.}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
    moderate amount of debugging information, or to 2 to get more.} 
}

\details{Different formulae are used for different instruments, e.g for
  RDI instruments, beams are rotated using formulae from page 13 and 14
  of \emph{RD Instruments} (1998).}

\value{An object with the \code{data$ma$v[,1:3,]} altered appropriately,
  and with \code{metadata$oce.orientation} changed from \code{xyz} to
  \code{enu}.}

\examples{
library(oce)
data(adp)
f <- adp.beam2xyz(adp)
e <- adp.xyz2enu(f)
plot(e, which=1:3)
}

\references{RD Instruments, 1998.
\emph{ADCP Coordinate Transformation, formulas and calculations.}
P/N 951-6079-00 (July 1998).}

\seealso{See \code{\link{read.adp}} for other functions that relate to objects
of class \code{"adp"}.}

\author{Dan Kelley}

\keyword{misc}
