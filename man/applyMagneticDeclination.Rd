\name{applyMagneticDeclination}

\alias{applyMagneticDeclination}
\title{Earth magnetic declination}

\description{Earth applyMagneticDeclination}

\usage{applyMagneticDeclination(x, declination=0, debug=getOption("oceDebug"))}

\arguments{
  \item{x}{an oce object.}
  \item{declination}{magnetic declination (to be added to the heading)}
  \item{debug}{a debugging flag, set to a positive value to get debugging.}
}

\details{Instruments that use magnetic compasses to determine current
  direction need to have corrections applied for magnetic declination,
  to get currents with the y component oriented to geographic, not
  magnetic, north.  Sometimes, and for some instruments, the declination
  is specified when the instrument is set up, so that the velocities as
  recorded are already.  Other times, the data need to be adjusted.
  This function is for the latter case.}

\value{Object, with velocity components ajusted to be aligned with
  geographic north and east.}

\seealso{Use \code{\link{magneticField}} to determine the declination,
    inclination and intensity at a given spot on the world, at a given time.}

\examples{
library(oce)
}

\references{\url{http://www.ngdc.noaa.gov/IAGA/vmod/igrf.html}}

\author{Dan Kelley}

\keyword{misc}

\concept{tide}
