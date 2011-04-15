\name{processingLog.summary}

\alias{processingLog.summary}

\title{Summarize the processingLog of an object}

\description{Summarize the processingLog of an object.}

\usage{processingLog.summary(object)}

\arguments{
\item{object}{an \code{oce} object.}
}


\details{Prints log entries with UTC timestamps.}

\value{None.}

\examples{
library(oce)
data(ctd)
processingLogSummary(ctd)
}

\author{Dan Kelley}

\keyword{misc}
