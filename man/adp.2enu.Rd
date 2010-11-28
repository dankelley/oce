\name{adp.2enu}

\alias{adp.2enu}

\title{Convert ADP to enu coordinates, from any other coordinates}

\description{Convert ADP velocities to enu coordinates, from any other
coordinates}

\usage{adp.2enu(x, declination=0, debug=getOption("oce.debug"))}

\arguments{
  \item{x}{an object of class \code{"adp"}.}
  \item{declination}{magnetic declination to be added to the heading, to
    get ENU with N as "true" north.}
  \item{debug}{a flag that, if non-zero, turns on debugging.  Higher
    values yield more extensive debugging.  This is passed to called
    functions, after subtracting 1.}
}

\details{This is mostly a wrapper that calls \code{\link{adp.beam2xyz}} if needed,
  and then calls \code{\link{adp.xyz2enu}}.}

\seealso{See \code{\link{read.adp}} for notes on functions relating to
  \code{"adp"} objects.  Also, see \code{\link{adp.beam2xyz}} and
  \code{\link{adp.xyz2enu}}.
}

\author{Dan Kelley}

\references{

\url{http://www.nortek-as.com/lib/forum-attachments/coordinate-transformation}

}

\keyword{misc}
