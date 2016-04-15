\name{time.oce}

\alias{time.oce}

\title{Extract time from an oce object}

\description{Extract time from an oce object}

\usage{\method{time}{oce}(x, \dots)}

\arguments{
  \item{x}{an \code{oce} object.}
  \item{\dots}{possibly contains an argument named \code{which}, as explained
      in \dQuote{Details}.}
}

\details{This is a convenience function that isolates the user from the details
    of details of storage.  The behaviour depends on the type of object.
    \itemize{

        \item \code{adv} objects: the times for the velocity observations are
        returned as a vector of POSIX times.
        
        \item \code{adp} objects: by default, the times of velocity
        observations are returned.  This is also the case if
        \code{which=1} is supplied in the \dots list.  If \code{which=2} is
        supplied in the \dots list, the times at which heading was measured
        are returned; for some instruments, these are not the same as the
        velocity times.
    }
}

\value{A vector of times.}

\examples{
library(oce)
data(adp)
cat("the first time is:", format(time(adp)[1]), "\n")
}

\seealso{Similar accessor functions are \code{\link{heading}}, \code{\link{pitch}}, \code{\link{roll}}.}

\author{Dan Kelley}

\keyword{misc}
