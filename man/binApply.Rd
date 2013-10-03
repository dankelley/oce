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
## temperature excess over freezing point
library(oce)
data(ctd90)
ctd <- subset(ctd90, pressure < 100)
Te <- function(d)
{
    Tf <<- swTFreeze(d[['salinity']], 0) # at surface
    mean(d[['temperature']] - Tf)
}
b <- binApply(ctd, "pressure", seq(0, 5000, 10), Te)
plot(b[,2], b[,1], type='l', ylim=rev(range(ctd[["pressure"]])), 
     xlab="T - Tfreeze", ylab="Pressure")
}

\author{Dan Kelley}

\keyword{misc}
