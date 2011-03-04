\name{number.as.POSIXct}

\alias{number.as.POSIXct}

\title{Convert an integer time to a POSIXct time}

\description{Convert an integer time to a POSIXct time}

\usage{number.as.POSIXct(t, type=c("unix", "matlab"), tz="UTC")}

\arguments{

    \item{t}{an integer corresponding to a time, in a way that depends on
        \code{type} (see \dQuote{Details}).}

    \item{type}{the type of time (see \dQuote{Details}).}

    \item{tz}{a string indicating the time zone.}

}

\details{Unix times, indicated by \code{type="unix"}, are measured in seconds
    since the start of the year 1970.  Matlab times, indicated by
    \code{type="matlab"}, are measured in years since the start of the year
    1899.  
    
    The starting point of time systems is a common source of confusion; for
    example, some would say that Matlab time starts in the year 1900, but that
    it starts at 1, not at 0.  Some light is cast on such issues in the
    \dQuote{Examples} section.)
    
    }
    
\value{A \code{\link{POSIXct}} time.}

\examples{
cat("In Unix, time=0 (seconds) means ", format(number.as.POSIXct(0)), "\n")
cat("In Matlab, time=1 (days) means ", format(number.as.POSIXct(1, type="matlab")), "\n")
}

\author{Dan Kelley}

\keyword{misc}
