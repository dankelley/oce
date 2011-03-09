\name{adv.beam2xyz}

\alias{adv.beam2xyz}

\title{Convert ADV from beam coordinates to xyz coordinates}

\description{Convert ADV velocity components from a beam-based
  coordinate system to a xyz-based coordinate system.}

\usage{adv.beam2xyz(x, debug=getOption("oce.debug"))}

\arguments{
  \item{x}{an object of class \code{"adv"}.}
  \item{debug}{a flag that, if non-zero, turns on debugging.  Higher
    values yield more extensive debugging.}
}

\details{The coordinate transformation is done using the transformation matrix
    contained in \code{x$metadata$transformation.matrix}, which is normally
    inferred from the header in the binary file.  If there is no such matrix
    (e.g. if the data were streamed through a data logger that did not capture
    the header), \code{adv.beam2xyz} the user will need to store one in
    \code{x}, e.g. by doing something like the following:
    \code{x$metadata$transformation.matrix <- rbind(c(11100, -5771,  -5321), c(
        291,  9716, -10002), c( 1409,  1409,   1409)) / 4096} .}

\seealso{See \code{\link{read.adv}} for notes on functions relating to
  \code{"adv"} objects.}

\author{Dan Kelley}

\references{
\url{http://www.nortek-as.com/lib/forum-attachments/coordinate-transformation}
}

\keyword{misc}
