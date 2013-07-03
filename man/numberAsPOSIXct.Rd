\name{numberAsPOSIXct}

\alias{numberAsPOSIXct}

\title{Convert a numeric time to a POSIXct time}

\description{Convert a numeric time to a POSIXct time}

\usage{numberAsPOSIXct(t, type=c("unix", "matlab", "gps", "argo", "sas", "spss"), tz="UTC")}

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
    1899.  Argo times, indicated by \code{type="argo"}, are measured in days
    since the start of the twentieth century.   SAS times, indicated by
    \code{type="sas"}, have origin at the start of 1960.  SPSS times, indicated
    by \code{type="spss"}, have origin at 1582-10-14.
    
    Note that in these cases, \code{t} is a vector.  The GPS case is different,
    however.  For \code{type="gps"}, \code{t} is a two-column matrix, in which
    the first column is the GPS "week" (referenced to 1999-08-22) and the
    second column is GPS "second" (i.e. the second within the week).  }
    
\value{A \code{\link{POSIXct}} time.}

\examples{
cat("In Unix, time=0 (seconds) means ", format(numberAsPOSIXct(0)), "\n")
cat("In Matlab, time=1 (days) means ", format(numberAsPOSIXct(1, type="matlab")), "\n")
cat("Canada Day 2001 was", format(numberAsPOSIXct(cbind(566,345615), type="gps")), "\n")
}

\author{Dan Kelley}

\keyword{misc}
