\name{integerToAscii}

\alias{integerToAscii}

\title{Decode integer to corresponding ASCII code}

\description{Decode integer to corresponding ASCII code}

\usage{integerToAscii(i)}

\arguments{
  \item{i}{an integer, or integer vector.}
}

\value{A character, or character vector.}

\examples{
library(oce)
A <- integerToAscii(65)
cat("A=", A, "\n")
}

\author{Dan Kelley}

\keyword{misc}
