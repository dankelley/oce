\name{subtractBottomVelocity}

\alias{subtractBottomVelocity}

\title{Subtract bottom velocity from ADP velocity}

\description{Subtract bottom velocity from ADP velocity}

\usage{subtractBottomVelocity(x, debug=getOption("oceDebug"))}

\arguments{
  \item{x}{an object of class \code{"adp"}, which contains bottom
  tracking velocities.}
  \item{debug}{a flag that, if non-zero, turns on debugging.  Higher
    values yield more extensive debugging.  This is passed to called
    functions, after subtracting 1.}
}

\details{Subtracts bottom tracking velocities from an \code{"adp"}
  object. Works for all coordinate systems (\code{beam}, \code{xyz}, and
  \code{enu}).}

\seealso{See \code{\link{read.adp}} for notes on functions relating to
    \code{"adp"} objects, and \code{\link{adp-class}} for notes on the ADP
    object class.}

\author{Dan Kelley and Clark Richards}

\keyword{misc}

