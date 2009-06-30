\name{adv.beam2frame}

\alias{adv.beam2frame}

\title{Convert ADV from beam coordinates to frame coordinates}

\description{Convert ADV velocity components from a beam-based
  coordinate system to a frame-based coordinate system.}

\usage{adv.beam2frame(x)}

\arguments{\item{x}{an object of class \code{"adv"}.}}

\details{The coordinate transformation is done using the matrix
  contained within the header.  To indicate the change, the value of
  \code{metadata$oce.orientation} is changed from \code{beam} to
  \code{frame}.}

\seealso{See \code{\link{read.adv}} for notes on functions relating to
  \code{"adv"} objects.}

\author{Dan Kelley}

\keyword{misc}
