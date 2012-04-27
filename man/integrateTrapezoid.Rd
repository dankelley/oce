\name{integrateTrapezoid}

\alias{integrateTrapezoid}

\title{Use trapezoidal integration}

\description{Estimate the integral of one-dimensional function using the trapezoidal rule.}

\usage{integrateTrapezoid(x, y, cumulative=FALSE)}

\arguments{
  \item{x}{x values}
  \item{y}{y values, with length (\code{n}, say) that must match that of \code{x}}

  \item{cumulative}{Flag indicating whether to return just the integral, or a
      vector of length \code{n} with 0 as the first element, the area between
      \code{x[2]} and \code{x[1]} as the second element, etc.}
}

\value{Either a single number representing the area under the \code{y=y(x)}
    function (if \code{cumulative=FALSE}), or a vector of length matching
    that of \code{x}, as described above.}

\examples{
x <- seq(0, 1, length.out=10) # try larger length.out to see if area approaches 2
y <- 2*x + 3*x^2
area <- integrateTrapezoid(x, y, FALSE)
darea <- integrateTrapezoid(x, y, TRUE)
print(area)
print(darea)
print(sum(darea))
}

\section{Bugs}{There is no handling of \code{NA} values.}

\author{Dan Kelley}

\keyword{misc}
