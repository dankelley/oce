\name{subtractBottomVelocity}

\alias{subtractBottomVelocity}

\title{Subtract bottom velocity from ADP velocity}

\description{Subtract bottom velocity from ADP velocity}

\usage{subtractBottomVelocity(x, debug=getOption("oceDebug"))}

\arguments{
  \item{x}{an object of class \code{"adp"}, which is in \emph{beam} coordinates.}
  \item{debug}{a flag that, if non-zero, turns on debugging.  Higher
    values yield more extensive debugging.  This is passed to called
    functions, after subtracting 1.}
}

\details{For now, the data must be in \emph{beam} coordinates.  This will be
    changed when requests are made for other coordinate systems.}

\seealso{See \code{\link{read.adp}} for notes on functions relating to
    \code{"adp"} objects, and \code{\link{adp-class}} for notes on the ADP
    object class.}

\author{Dan Kelley}

\keyword{misc}

