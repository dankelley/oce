\name{enuToOtherAdv}

\alias{enuToOtherAdv}

\title{Convert east-north-up to other coordinate}

\description{Convert ADV velocity components from an enu-based
  coordinate system to another system, perhaps to align axes with the
  coastline.}

\usage{enuToOtherAdv(x, heading=0, pitch=0, roll=0, debug=getOption("oceDebug"))}

\arguments{
  \item{x}{an object of class \code{"adv"}.}
  \item{heading}{number or vector of numbers, giving the angle, in degrees, to
      be added to the heading.  See \dQuote{Details}.}
  \item{pitch}{as \code{heading} but for pitch.}
  \item{roll}{as \code{heading} but for roll.}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
    debugging information.} 
}

\details{The supplied angles specify rotations to be made around the
  axes for which heading, pitch, and roll are defined.  For example, an
  eastward current will point southeast if \code{heading=45} is used.
  
  The returned value has heading, pitch, and roll matching those of \code{x},
  so these angles retain their meaning as the instrument orientation.

  NOTE: this function works similarly to \code{\link{xyzToEnuAdv}}, except
  that in the present function, it makes no difference whether the instrument
  points up or down, etc.
}

\seealso{See \code{\link{read.adv}} for other functions that relate to objects
    of class \code{"adv"}}

\author{Dan Kelley}

\keyword{misc}
