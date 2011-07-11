\name{processingLog}

\alias{processingLog}
\alias{processingLog<-}
\alias{processingLogItem}

\title{Add an item to object processingLog}

\description{Add an item to object processingLog}

\usage{
processingLog(x) <- value
processingLog(h, value="")
processingLogItem(value="")
}

\arguments{
  \item{h}{a processingLog, e.g. \code{x$processingLog} for an \code{oce} object named \code{x}. 
      If \code{h} is \code{NULL}, a new processingLog is created.}
  \item{value}{a character string describing the action.}
  \item{x}{an \code{oce} object.}
}

\details{The \code{processingLog} function is designed for the user to 
    alter the processingLog of an object.  The \code{processingLogItem}
    function is used internally within the package.}

\value{The updated processingLog.}

\examples{
data(ctd)
ctd$metadata$latitude <- ctd$metadata$latitude + 1
processingLog(ctd) <- "correct the latitude"
}

\author{Dan Kelley}

\keyword{misc}
