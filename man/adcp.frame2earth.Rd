\name{adcp.frame2earth}

\alias{adcp.frame2earth}

\title{Convert ADCP from frame coordinates to earth coordinates}

\description{Convert ADCP velocity components from a frame-based
  coordinate system (u, v, w, e) to an earth-based coordinate system
  (east, north, up, e).}

\usage{adcp.frame2earth(x, pitch, heading, roll)}

\arguments{
  \item{x}{an object of class \code{"adcp"}}
  \item{pitch}{a number indicating the pitch of the instrument frame, in
    degrees. If not supplied, which is the norm, this is inferred from
    \code{x}, on a profile-by-profile basis.}
  \item{heading}{as \code{pitch}, but for the heading of the instrument
    frame.}
  \item{roll}{as \code{pitch}, but for the roll of the instrument frame.}
}

\details{Beams are converted to velocity components using formulae
  from page 13 and 14 of \emph{RD Instruments} (1998).  Note that for
  downward-facing instruments (i.e. for
  \code{x$metadata$orientation="down"}), pitch and roll are multiplied
  by -1 before the calculation is done.}

\value{An object of \code{\link[base]{class}} \code{"adcp"}, with
  columns renamed from \code{u}, \code{v} and \code{w} to \code{east},
  \code{north}, and \code{up}.  (Note that \code{e} is left unaltered.)
  Also, \code{metadata$oce.orientation} is changed from \code{frame} to
  \code{earth}.}

\examples{
library(oce)
data(adcp)
f <- adcp.beam2frame(adcp)
e <- adcp.frame2earth(f)
plot(e, which=1:3)
}

\references{
RD Instruments, 1998.
\emph{ADCP Coordinate Transformation, formulas and calculations.}
P/N 951-6079-00 (July 1998)
}


\author{Dan Kelley}

\keyword{misc}
