\name{adv.earth2other}

\alias{adv.earth2other}

\title{Convert ADV from beam coordinates to earth coordinates}

\description{Convert ADV velocity components from a beam-based
  coordinate system to an earth-based coordinate system.}

\usage{adv.earth2other(x, heading=0, pitch=0, roll=0)}

\arguments{
  \item{x}{an object of class \code{"adv"}.}
  \item{heading}{angle, in degrees, to be added to the
    heading.}
  \item{pitch}{angle to be added to the pitch.}
  \item{roll}{angle to be added to the roll.}
}

\details{The value of \code{x$metadata$oce.coordinate} is set to
  \code{"other"} to indicate this change in coordinate system.}

\seealso{See \code{\link{read.adv}} for notes on functions relating to
  \code{"adv"} objects.}

\author{Dan Kelley}

\keyword{misc}
