\name{integrateTrapezoid}

\alias{integrateTrapezoid}

\title{Use trapezoidal integration}

\description{Estimate the integral of one-dimensional function using the trapezoidal rule.}

\usage{integrateTrapezoid(x, y, type=c("A", "dA", "cA"))}

\arguments{
  \item{x}{x values, or a single value, \eqn{dx}{dx}, indicating the difference
      between x values.  If \code{x} is not given, \eqn{dx}{dx} will be taken
      to equal 1.}
  \item{y}{y values, with length (\code{n}, say) that must match that of \code{x}}
  \item{type}{Flag indicating the desired return value (see \dQuote{Value}).}
}

\value{If \code{type="A"} (the default), a single value is returned, containing
    the estimate of the integral of \code{y=y(x)}.  If \code{type="dA"}, a
    numeric vector of the same length as \code{x}, of which the first element
    is zer0, the second element is the integral between \code{x[1]} and
    \code{x[2]}, etc.   If \code{type="cA"}, the result is the cumulative sum
    (as in \code{\link{cumsum}}) of the values that would be returned for
    \code{type="dA"}.  See \dQuote{Examples}.}

\examples{
x <- seq(0, 1, length.out=10) # try larger length.out to see if area approaches 2
y <- 2*x + 3*x^2
A <- integrateTrapezoid(x, y)
dA <- integrateTrapezoid(x, y, "dA")
cA <- integrateTrapezoid(x, y, "cA")
print(A)
print(sum(dA))
print(tail(cA, 1))
print(integrateTrapezoid(diff(x[1:2]), y))
print(integrateTrapezoid(y=y))
}

\section{Bugs}{There is no handling of \code{NA} values.}

\author{Dan Kelley}

\keyword{misc}
