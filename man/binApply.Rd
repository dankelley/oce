\name{binApply}

\alias{binApply}

\title{Divide an oce object or a data frame into bins, and apply a
    user-supplied function in each}

\description{Divide an oce object or a data frame into bins, and apply a
    user-supplied function in each}

\usage{binApply(x, v, b, f, \dots)}

\arguments{

  \item{x}{an oce object, a data frame, or something that can be coerced into a
      data frame.}

  \item{v}{name of a variable in \code{x}.}

  \item{b}{sequence of bins into which categorize \code{v}.}

  \item{f}{a function that is to be applied to the subdivided \code{x}.}

  \item{\dots}{optional arguments passed to \code{f}.}

}

\details{First, a data frame is extracted from \code{x}.  If \code{x} is an Oce
    object, this is done by coercing \code{x@data} as a data frame.  If not,
    then \code{x} itself is coerced.  An error results if coercion is not
    possible.  Next, \code{\link{findInterval}} is used on \code{x[[v]]}, using
    \code{b} to define the intervals.  Then, the function \code{f} is applied
    to the subsets of \code{x} corresponding to the intervals.  The values
    returned by the calls to \code{f} are collected as the rows of a matrix.
    Finally, the mean values of \code{x[[v]]} are computed for each interval,
    and the resultant vector is attached as the first column of the matrix, and
    this matrix is returned.}
    
\value{A matrix with first column equal to the mean values of \code{x[[v]]} in
    each subdivision, followed by columns for the return value from \code{f}.
    For example, if \code{f} returns a single value, a two-column result is
    returned by \code{binApply}.}

\examples{
## profile of conf. interval about temperature mean
library(oce)
data(tdr)
f <- function(d)
    t.test(d$temperature)$conf.int
b <- binApply(tdr, "pressure", seq(0,20,2.5), f)
plot(0.5*(b[,2]+b[,3]), b[,1], xlab="T", ylab="p",
     xlim=range(b[,2:3]), ylim=c(20,0), type='p')
segments(b[,2], b[,1], b[,3], b[,1])
}

\author{Dan Kelley}

\keyword{misc}
