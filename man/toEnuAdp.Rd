\name{toEnuAdp}

\alias{toEnuAdp}

\title{Convert from ENU coordinates}

\description{Convert ADP velocities to enu coordinates, from any other
coordinates}

\usage{toEnuAdp(x, declination=0, debug=getOption("oceDebug"))}

\arguments{
  \item{x}{an object of class \code{"adp"}.}
  \item{declination}{magnetic declination to be added to the heading, to
    get ENU with N as "true" north.}
  \item{debug}{a flag that, if non-zero, turns on debugging.  Higher
    values yield more extensive debugging.  This is passed to called
    functions, after subtracting 1.}
}

\details{This is mostly a wrapper that calls \code{\link{beamToXyzAdp}} if needed,
  and then calls \code{\link{xyzToEnuAdp}}.}

\seealso{See \code{\link{read.adp}} for notes on functions relating to
  \code{"adp"} objects.  Also, see \code{\link{beamToXyzAdp}} and
  \code{\link{xyzToEnuAdp}}.
}

\author{Dan Kelley}

\references{

\url{http://www.nortek-as.com/lib/forum-attachments/coordinate-transformation}

}

\keyword{misc}
