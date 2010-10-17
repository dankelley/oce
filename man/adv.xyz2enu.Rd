\name{adv.xyz2enu}

\alias{adv.xyz2enu}

\title{Convert ADV from xyz coordinates to enu coordinates}

\description{Convert ADV velocity components from a xyz-based
  coordinate system to an enu-based coordinate system.}

\usage{adv.xyz2enu(x, declination=0, debug=getOption("oce.debug"))}

\arguments{
  \item{x}{an object of class \code{"adv"}.}
  \item{declination}{magnetic declination to be added to the heading, to
    get ENU with N as "true" north.}
  \item{debug}{a flag that, if non-zero, turns on debugging.  Higher
    values yield more extensive debugging.}
}

\details{The coordinate transformation is done using the heading, pitch,
  and roll information contained within \code{x}.  The algorithm is
  similar to that used for Teledyne/RDI ADCP units, taking into account
  the different definitions of heading in the two cases.

  Generally, the transformation must be done on a time-by-time basis,
  which is a slow operation.  However, this function checks whether the
  vectors for heading, pitch and roll, are all of unit length, and in
  that case, the calculation time is reduced by an order of magnitude.
  This situation of singleton vectors is not uncommon, because the
  orientation data can be noisy, requiring the user to substitute
  cleaned-up values, and if the instrument is moored on a steady
  platform, it makes sense to supply single values.  Note that the
  angles are held in \code{data$ts.slow} for Nortek instruments and
  \code{data$ts} for Sontek instruments.

  To indicate the change in coordinate, the value of
  \code{metadata$oce.orientation} is changed from \code{xyz} to
  \code{enu}, and this is noticed by e.g. \code{\link{plot.adv}}.}

\seealso{See \code{\link{read.adv}} for notes on functions relating to
  \code{"adv"} objects.}

\author{Dan Kelley}

\references{Nortek coordinate transformaton matlab script
(\url{http://www.nortek-as.com/lib/forum-attachments/coordinate-transformation})
}


\keyword{misc}
