\name{processing.log.summary}

\alias{processing.log.summary}

\title{Summarize the processing log of an object}

\description{Summarize the processing log of an object.}

\usage{processing.log.summary(object)}

\arguments{
\item{object}{an \code{oce} object.}
}


\details{Prints log entries with UTC timestamps.}

\value{None.}

\examples{
library(oce)
data(ctd)
processing.log.summary(ctd)
}

\author{Dan Kelley}

\keyword{misc}
