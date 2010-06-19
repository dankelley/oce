\name{adv.xyz2enu}

\alias{adv.xyz2enu}

\title{Convert ADV from xyz coordinates to enu coordinates}

\description{Convert ADV velocity components from a xyz-based
  coordinate system to an enu-based coordinate system.}

\usage{adv.xyz2enu(x)}

\arguments{\item{x}{an object of class \code{"adv"}.}}

\details{The coordinate transformation is done using the heading, pitch,
  and roll information contained within \code{x}.  To indicate the
  change, the value of \code{metadata$oce.orientation} is changed from
  \code{xyz} to \code{enu}.}

\seealso{See \code{\link{read.adv}} for notes on functions relating to
  \code{"adv"} objects.}

\author{Dan Kelley}

\references{

1. Nortek coordinate transformaton matlab script
(\url{http://www.nortek-as.com/lib/forum-attachments/coordinate-transformation})

}


\keyword{misc}
