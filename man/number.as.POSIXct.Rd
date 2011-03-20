\name{number.as.POSIXct}

\alias{number.as.POSIXct}

\title{Convert an integer time to a POSIXct time}

\description{Convert an integer time to a POSIXct time}

\usage{number.as.POSIXct(t, type=c("unix", "matlab", "gps"), tz="UTC")}

\arguments{

    \item{t}{an integer corresponding to a time, in a way that depends on
        \code{type} (see \dQuote{Details}).}

    \item{type}{the type of time (see \dQuote{Details}).}

    \item{tz}{a string indicating the time zone, used only for unix and matlab
        times, since GPS times are always referenced to the UTC timezone.}

}

\details{Unix times, indicated by \code{type="unix"}, are measured in seconds
    since the start of the year 1970.  Matlab times, indicated by
    \code{type="matlab"}, are measured in years since the start of the year
    1899.  Thus, in both cases, \code{t} is a vector.  However, for
    \code{type="gps"}, \code{t} is a two-column matrix, in which the first
    column is the GPS "week" (referenced to 1999-08-22) and the second column
    is GPS "second" (i.e. the second within the week).
    
    }
    
\value{A \code{\link{POSIXct}} time.}

\examples{
cat("In Unix, time=0 (seconds) means ", format(number.as.POSIXct(0)), "\n")
cat("In Matlab, time=1 (days) means ", format(number.as.POSIXct(1, type="matlab")), "\n")
}

\author{Dan Kelley}

\keyword{misc}
