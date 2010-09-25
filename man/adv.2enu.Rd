\name{adv.2enu}

\alias{adv.2enu}

\title{Convert ADV to enu coordinates, from any other coordinates}

\description{Convert ADV velocities to enu coordinates, from any other
coordinates}

\usage{adv.2enu(x, declination=0, debug=getOption("oce.debug"))}

\arguments{
  \item{x}{an object of class \code{"adv"}.}
  \item{declination}{magnetic declination to be added to the heading, to
    get ENU with N as "true" north.}
  \item{debug}{a flag that, if non-zero, turns on debugging.  Higher
    values yield more extensive debugging.  This is passed to called
    functions, after subtracting 1.}
}

\details{This is mostly a wrapper that calls \code{\link{adv.beam2xyz}} if needed,
  and then calls \code{\link{adv.xyz2enu}}.}

\seealso{See \code{\link{read.adv}} for notes on functions relating to
  \code{"adv"} objects.  Also, see \code{\link{adv.beam2xyz}} and
  \code{\link{adv.xyz2enu}}.
}

\author{Dan Kelley}

\references{

1. Nortek coordinate transformaton matlab script
(\url{http://www.nortek-as.com/lib/forum-attachments/coordinate-transformation})

}

\keyword{misc}
