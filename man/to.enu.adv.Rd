\name{to.enu.adv}

\alias{to.enu.adv}

\title{Convert ADV to enu coordinates, from any other coordinates}

\description{Convert ADV velocities to enu coordinates, from any other
coordinates}

\usage{to.enu.adv(x, declination=0, debug=getOption("oce.debug"))}

\arguments{
  \item{x}{an object of class \code{"adv"}.}
  \item{declination}{magnetic declination to be added to the heading, to
    get ENU with N as "true" north.}
  \item{debug}{a flag that, if non-zero, turns on debugging.  Higher
    values yield more extensive debugging.  This is passed to called
    functions, after subtracting 1.}
}

\details{This is mostly a wrapper that calls \code{\link{beam.to.xyz.adv}} if needed,
  and then calls \code{\link{xyz.to.enu.adv}}.}

\seealso{See \code{\link{read.adv}} for notes on functions relating to
  \code{"adv"} objects.  Also, see \code{\link{beam.to.xyz.adv}} and
  \code{\link{xyz.to.enu.adv}}.
}

\author{Dan Kelley}

\references{

\url{http://www.nortek-as.com/lib/forum-attachments/coordinate-transformation}

}

\keyword{misc}
