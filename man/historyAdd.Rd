\name{historyAdd}

\alias{historyAdd}

\title{Add an item to object history}

\description{Add an item to object history}

\usage{historyAdd(h, action="")}

\arguments{
  \item{h}{a historyIf \code{NULL}, a new history is created.}
  \item{action}{a character string describing the action.}
}

\details{This is used to add a time-stamped item to the history of the object.}

\value{The updated history.}

\examples{
data(ctd)
ctd$metadata$latitude <- ctd$metadata$latitude + 1
ctd$history <- historyAdd(ctd$history, "correct latitude")
}

\author{Dan Kelley}

\keyword{misc}
