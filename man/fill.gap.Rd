\name{fill.gap}

\alias{fill.gap}

\title{Fill a gap in an oce object}

\description{Fill a gap in an oce object}

\usage{fill.gap(x, method=c("linear"))}

\arguments{
  \item{x}{an \code{oce} object.}
  \item{method}{to use; see \dQuote{Details}.}
}

\details{Sequences of \code{NA} values, are filled by linear
  interpolation between the non-\code{NA} values that bound the gap.}

\section{Bugs}{\itemize{
    \item Eventually, this will be expanded to work with any
    \code{oce} object.  But, for now, it only works for vectors that can
    be coerced to numeric.
    \item If the first or last point is \code{NA}, then \code{x}
    is returned unaltered.}}

\value{A new \code{oc} object, with gaps removed.}

\examples{
library(oce)
# Integers
x <- c(1:2, NA, NA, 5:6)
y <- fill.gap(x)
print(data.frame(x,y))
# Floats
x <- x + 0.1
y <- fill.gap(x)
print(data.frame(x,y))
}

\author{Dan Kelley}

\keyword{misc}
