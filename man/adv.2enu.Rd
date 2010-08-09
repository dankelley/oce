\name{adv.2enu}

\alias{adv.2enu}

\title{Convert ADV to enu coordinates, from any other coordinates}

\description{Convert ADV velocities to enu coordinates, from any other
coordinates}

\usage{adv.2enu(x)}

\arguments{\item{x}{an object of class \code{"adv"}.}}

\details{The coordinate transformation is done using the matrix
  contained within the header.  To indicate the change, the value of
  \code{metadata$oce.orientation} is set to \code{enu}; note that the
  \code{metadata$coordinate.system} is \emph{not} changed.}

\seealso{See \code{\link{read.adv}} for notes on functions relating to
  \code{"adv"} objects.}

\author{Dan Kelley}

\references{

1. Nortek coordinate transformaton matlab script
(\url{http://www.nortek-as.com/lib/forum-attachments/coordinate-transformation})

}

\keyword{misc}
