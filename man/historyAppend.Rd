\name{historyAppend}

\alias{historyAppend}

\title{Copy an object, but with an updated history.}

\description{Copy an object, but with an updated history.}

\usage{historyAppend(x, action="")}

\arguments{
  \item{x}{an \code{oce} object.}
  \item{action}{a character string describing the action.}
}

\details{This returns an object with an updated history.}

\value{A copy of the object, but with the history containing the new entry.}

\examples{
data(ctd)
ctd$metadata$latitude <- ctd$metadata$latitude + 1
ctd <- historyAppend(ctd, "correct latitude")
}

\author{Dan Kelley}

\keyword{misc}
