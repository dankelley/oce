\name{findBottom}
\alias{findBottom}
\title{Find the ocean bottom in an echosounder object}
\description{Find the ocean bottom in an echosounder object}

\usage{findBottom(x, ignore=5, clean=despike, \dots)}

\arguments{
  \item{x}{an object of class \code{echosounder}}
  \item{ignore}{number of metres of data to ignore, near the surface}
  \item{clean}{a function to clean the inferred depth of spikes}
  \item{\dots}{extra arguments passed along to \code{clean}.  For example, with
      the default value of \code{clean} as the function \code{\link{despike}},
      it might make sense to supply \code{k} to set the length of the running
      median filter.}
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
