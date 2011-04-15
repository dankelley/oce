\name{processingLogAppend}

\alias{processingLogAppend}

\title{Copy an object, but with an updated processing log.}

\description{Append an item to a processing log}

\usage{processingLogAppend(x, action="")}

\arguments{
  \item{x}{an \code{oce} object.}
  \item{action}{a character string describing the action.}
}

\details{This returns an object with an updated processing log.}

\value{A copy of the object, but with the processing log containing the
  new entry.}

\examples{
data(ctd)
ctd$metadata$latitude <- ctd$metadata$latitude + 1
ctd <- processingLogAppend(ctd, "correct latitude")
}

\author{Dan Kelley}

\keyword{misc}
