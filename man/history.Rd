\name{history}

\alias{history}
\alias{history<-}
\alias{historyItem}

\title{Add an item to object history}

\description{Add an item to object history}

\usage{
history(x) <- value
history(h, value="")
historyItem(value="")
}

\arguments{
  \item{h}{a history, e.g. \code{x$history} for an \code{oce} object named \code{x}. 
      If \code{h} is \code{NULL}, a new history is created.}
  \item{value}{a character string describing the action.}
  \item{x}{an \code{oce} object.}
}

\details{The \code{history} function is designed for the user to add items to
    the history of an object.  The \code{historyItem} function is used
    internally within the package.}

\value{The updated history.}

\examples{
data(ctd)
ctd$metadata$latitude <- ctd$metadata$latitude + 1
history(ctd) <- "correct the latitude"
}

\author{Dan Kelley}

\keyword{misc}
