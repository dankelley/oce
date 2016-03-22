\name{beamName}

\alias{beamName}

\title{Name an Acoustic-Doppler Beam}

\description{Name a beam for an acoustic-doppler device, adjusting for
  beam number and for coordinate system.}

\usage{beamName(x, which)}

\arguments{
  \item{x}{an acoustic-doppler object, inheriting from class
    \code{"adp"} or \code{"adv"}.}
  \item{which}{an integer indicating beam number.}
}

\value{A character string containing a reasonable name for the beam, of
  the form \code{"beam 1"}, etc., for beam coordinates, \code{"east"},
  etc. for enu coordinates, \code{"u"}, etc. for \code{"xyz"}, or
  \code{"u'"}, etc., for \code{"other"} coordinates.  The coordinate is
  determined by \code{x@metadata$oce.coordinate}.}


\seealso{This is used by \code{\link{read.oce}}.}

\author{Dan Kelley}

\keyword{misc}
