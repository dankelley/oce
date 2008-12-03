\name{header}

\alias{header}

\title{Return the header for an \code{oce} data object}

\description{Return the header for an \code{oce} data  object.}

\usage{
header(x)
}

\arguments{
  \item{x}{An \code{oce} object.}
}

\details{Returns \code{x$metadata$header}.}

\value{None}

\examples{
library(oce)
data(ctd)
header(ctd)
}

\author{Dan Kelley}

\keyword{misc}
