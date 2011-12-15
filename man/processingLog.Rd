\name{processingLog}

\alias{processingLog}
\alias{processingLog<-}
\alias{processingLogItem}
\alias{processingLogShow}

\title{Add an item to object processingLog}

\description{Add an item to object processingLog}

\usage{
processingLog(x) <- value
x[["processingLog"]] <- value
processingLog(h, value="")
processingLogItem(value="")
processingLogShow(x)
}

\arguments{

  \item{h}{an oce object, a processingLog, or \code{NULL}.  If \code{h} is an
      oce object, then \code{processingLog} returns a summary of the processing
      log, and \code{value} is ignored.  If \code{h} is a processing log, then
      \code{value} is added to it.  If \code{h} is \code{NULL}, then a new
      processing log is created, with \code{value} as the single item.}

  \item{value}{a character string describing the action.} \item{x}{an
      \code{oce} object.} }

\details{The \code{processingLog} function is designed for the user to 
    alter the processingLog of an object.  The \code{processingLogItem}
    function is used internally within the package.}

\value{The updated processingLog.}

\examples{
data(ctd)
ctd@metadata$latitude <- ctd@metadata$latitude + 1
processingLog(ctd) <- "correct the latitude"
}

\author{Dan Kelley}

\keyword{misc}
