% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{fillGap}
\alias{fillGap}
\title{Fill a Gap in an oce Object}
\usage{
fillGap(x, method = c("linear"), rule = 1)
}
\arguments{
\item{x}{an \linkS4class{oce} object.}

\item{method}{to use; see \dQuote{Details}.}

\item{rule}{integer controlling behaviour at start and end of \code{x}.  If
\code{rule=1}, \code{NA} values at the ends are left in the return value.
If \code{rule=2}, they are replaced with the nearest non-NA point.}
}
\value{
A new \code{oce} object, with gaps removed.
}
\description{
Sequences of \code{NA} values, are filled by linear interpolation between
the non-\code{NA} values that bound the gap.
}
\section{Bugs}{

\enumerate{
\item Eventually, this will be expanded to work
with any \code{oce} object.  But, for now, it only works for vectors that
can be coerced to numeric.
\item If the first or last point is \code{NA}, then \code{x} is returned unaltered.
\item Only method \code{linear} is permitted now.
}
}

\examples{
library(oce)
# Integers
x <- c(1:2, NA, NA, 5:6)
y <- fillGap(x)
print(data.frame(x, y))
# Floats
x <- x + 0.1
y <- fillGap(x)
print(data.frame(x, y))
}
\author{
Dan Kelley
}
