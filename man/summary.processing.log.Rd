\name{summary.processing.log}

\alias{summary.processing.log}

\title{Summarize the processing log of an object}

\description{Summarize the processing log of an object.}

\usage{summary.processing.log(object)}

\arguments{
\item{object}{an \code{oce} object.}
}


\details{Prints log entries with UTC timestamps.}

\value{None.}

\examples{
library(oce)
data(ctd)
summary.processing.log(ctd)
}

\author{Dan Kelley}

\keyword{misc}
