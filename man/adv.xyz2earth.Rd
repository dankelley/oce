\name{adv.xyz2earth}

\alias{adv.xyz2earth}

\title{Convert ADV from xyz coordinates to earth coordinates}

\description{Convert ADV velocity components from a xyz-based
  coordinate system to an earth-based coordinate system.}

\usage{adv.xyz2earth(x)}

\arguments{\item{x}{an object of class \code{"adv"}.}}

\details{The coordinate transformation is done using the heading, pitch,
  and roll information contained within \code{x}.  To indicate the
  change, the value of \code{metadata$oce.orientation} is changed from
  \code{xyz} to \code{earth}.}

\seealso{See \code{\link{read.adv}} for notes on functions relating to
  \code{"adv"} objects.}

\author{Dan Kelley}

\keyword{misc}
