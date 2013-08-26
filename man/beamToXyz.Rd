\name{beamToXyz}

\alias{beamToXyz}
\alias{xyzToEnu}
\alias{enuToOther}
\alias{toEnu}

\title{Change ADV or ADP coordinate systems}

\description{Convert velocity data from an acoustic-doppler velocimeter or
    acoustic-doppler profiler from one coordinate system to another.}

\usage{beamToXyz(x, \dots)
xyzToEnu(x, \dots)
enuToOther(x, \dots)
toEnu(x, \dots)
}

\arguments{
  \item{x}{an object of class \code{"adv"} or \code{"adp"}.}
  \item{\dots}{extra arguments that are passed on to the called function.}
}

\value{An object of the same type as \code{x}, but with \code{x[["v"]]}
    converted from beam coordinates to xyz coordinates, and with
    \code{x[["oceCoordinate"]]} changed from \code{"beam"} to \code{"xyz"}.}

\details{Each of these functions checks the type of object, and calls the
    corresponding function, as appropriate.  For example, \code{beamToXyz}
    calls \code{\link{beamToXyzAdp}} for an object that inhertis from
    \code{"adp"} or \code{\link{beamToXyzAdv}} for an object that inhertis from
    \code{"adv"}.}

\seealso{The real work is done with specialized routines, 
    \code{\link{beamToXyzAdp}}, \code{\link{beamToXyzAdv}},
    \code{\link{xyzToEnuAdp}}, \code{\link{xyzToEnuAdv}},
    \code{\link{enuToOtherAdp}}, \code{\link{enuToOtherAdv}},
    \code{\link{toEnuAdp}}, and \code{\link{toEnuAdv}}.}

\author{Dan Kelley}

\keyword{misc}
