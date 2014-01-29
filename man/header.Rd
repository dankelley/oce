\name{header}

\alias{header}

\title{Return the header for an Oce data object}

\description{Return the header for an oce data  object.}

\usage{header(x)}

\arguments{
  \item{x}{an \code{oce} object.}
}

\details{Returns header, taken from the metadata of x.}

\value{The header, often as a list of character strings.}

\examples{
library(oce)
if (require(ocedata)) {
    data(ctd)
    header(ctd)
}
}

\author{Dan Kelley}

\keyword{misc}
