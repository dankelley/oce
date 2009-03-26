\name{adcp.frame2earth}

\alias{adcp.frame2earth}

\title{Convert ADCP from frame coordinates to earth coordinates}

\description{Convert ADCP from frame coordinates to earth coordinates}

\usage{adcp.frame2earth(x, pitch, heading, roll)}

\arguments{
  \item{x}{an object of class \code{"adcp"}}
  \item{pitch}{a number indicating the pitch of the instrument frame, in
    degrees. If not supplied, which is the norm, this is inferred from
    \code{x}.}
  \item{heading}{a number indicating the heading of the instrument
    frame, in degrees.  If not supplied, which is the norm, this is
    inferred from \code{x}.}
  \item{roll}{a number indicating the pitch of the instrument frame, in
    degrees.  If not supplied, which is the norm, this is inferred from
    \code{x}.}
}

\details{Beams are converted to velocity components using formulae from
  page 13 and 14 of RD Instruments (1998).}

\value{An object of \code{\link[base]{class}} \code{"adcp"}.}

\examples{
\dontrun{
library(oce)
b <- read.oce(file = "~/SL08F001.000", skip = 5000, read = 6 * 30)
f <- adcp.beam2frame(b)
e <- adcp.frame2earth(f)
plot(e, which=1:3)
}
}

\references{
RD Instruments, 1998.
ADCP Coordinate Transformation
P/N 951-6079-00 (July 1998)
}


\author{Dan Kelley}

\keyword{misc}
