\name{integer2ascii}

\alias{integer2ascii}

\title{Decode integer to corresponding ASCII code}

\description{Decode integer to corresponding ASCII code}

\usage{integer2ascii(i)}

\arguments{
  \item{i}{an integer, or integer vector.}
}

\value{A character, or character vector.}

\examples{
library(oce)
A <- integer2ascii(65)
cat("A=", A, "\n")
}

\author{Dan Kelley}

\keyword{misc}
