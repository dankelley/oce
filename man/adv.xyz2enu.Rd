\name{adv.xyz2enu}

\alias{adv.xyz2enu}

\title{Convert ADV from xyz coordinates to enu coordinates}

\description{Convert ADV velocity components from a xyz-based
  coordinate system to an enu-based coordinate system.}

\usage{adv.xyz2enu(x, debug=getOption("oce.debug"))}

\arguments{
  \item{x}{an object of class \code{"adv"}.}
  \item{debug}{a flag that, if non-zero, turns on debugging.  Higher
    values yield more extensive debugging.}
}

\details{The coordinate transformation is done using the heading,
  pitch, and roll information contained within \code{x}.  The
  algorithm is similar to that used for Teledyne/RDI ADCP units,
  except that the heading is defined along the x axis.  Normally, the
  transformation is done on a time-by-time basis, which is a slow
  operation.  However, if there is only value in the vectors for
  heading, etc., then a faster calculation is done.

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
