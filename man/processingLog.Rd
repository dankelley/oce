\name{processingLog}

\alias{processingLog}
\alias{processingLog<-}
\alias{processingLogAppend}
\alias{processingLogItem}
\alias{processingLogShow}

\title{Add an item to object processingLog}

\description{Add an item to object processingLog}

\usage{
processingLog(x) <- value
processingLog(h, value="")
processingLogAppend(h, value="")
processingLogItem(value="")
processingLogShow(x)
}

\arguments{

  \item{h}{a processingLog, e.g. \code{x@processingLog}, if \code{x} is an object
      of \code{\link{oce-class}}.}

  \item{value}{a character string describing the action.} \item{x}{an
      \code{oce} object.} }

\details{The \code{processingLog} function is designed for the user to 
    alter the processingLog of an object.  The \code{processingLogItem}
    function is used internally within the package.}

\value{The updated processingLog.}

\examples{
library(oce)
data(ctd)
ctd@metadata$latitude <- ctd@metadata$latitude + 1
processingLog(ctd) <- "correct the latitude"
}

\author{Dan Kelley}

\keyword{misc}
