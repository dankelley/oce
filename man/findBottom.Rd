\name{findBottom}
\alias{findBottom}
\title{Find the ocean bottom in an echosounder object}
\description{Find the ocean bottom in an echosounder object}

\usage{findBottom(x, clean=despike)}

\arguments{
  \item{x}{an object of class \code{echosounder}}
  \item{clean}{a function to clean the inferred depth of spikes}
}

\details{Finds the depth in a Biosonics echosounder file, by finding the
    strongest reflector and smoothing its trace.}

\value{A list with elements: the \code{time} of a ping, the \code{depth} of the
    inferred depth in metres, and the \code{index} of the inferred bottom
    location, referenced to the object's \code{depth} vector.}

\seealso{The documentation for \code{\link{echosounder-class}} explains the
    structure of \code{echosounder} objects, and also outlines the other
    functions dealing with them.}

\author{Dan Kelley}

\keyword{misc}
