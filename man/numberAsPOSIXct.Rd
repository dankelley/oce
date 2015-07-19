% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{numberAsPOSIXct}

\alias{numberAsPOSIXct}

\title{Convert a numeric time to a POSIXct time}

\description{Convert a numeric time to a POSIXct time}

\usage{numberAsPOSIXct(t, type=c("unix", "matlab", "gps", "argo",
                          "ncep", "sas", "spss", "yearday"), tz="UTC")}

\arguments{

  \item{t}{an integer corresponding to a time, in a way that depends on
    \code{type}.}

  \item{type}{the type of time (see \dQuote{Details}).}

  \item{tz}{a string indicating the time zone, used only for unix and matlab
    times, since GPS times are always referenced to the UTC timezone.}

}

\details{Unix times, indicated by \code{type="unix"}, are measured in seconds
  since the start of the year 1970.  Matlab times, indicated by
  \code{type="matlab"}, are measured in years since the start of the year 1899.
  Argo times, indicated by \code{type="argo"}, are measured in days since the
  beginning of the year 1900.  NCEP times, indicated by \code{type="ncep"}, are
  measured in hours since the beginning of year 1800. SAS times, indicated by
  \code{type="sas"}, have origin at the start of 1960.  SPSS times, indicated by
  \code{type="spss"}, have origin at 1582-10-14.
    
  Note that in these cases, \code{t} is a vector.  In the yearday and GPS cases,
  however, \code{t} must be a two-column matrix.  For \code{type="gps"}, the
  first column is the GPS "week" (referenced to 1999-08-22) and the second
  column is GPS "second" (i.e. the second within the week).  For
  \code{type="yearday"}, the first column is the year, and the second is the
  yearday (starting at 1 for the first second of January 1, to match the
  convention used by Sea-Bird CTD software).}
    
\value{A \code{\link{POSIXct}} time vector.}

\examples{
numberAsPOSIXct(0)                     # unix time 0
numberAsPOSIXct(1, type="matlab")      # matlab time 1
numberAsPOSIXct(cbind(566,345615), type="gps") # Canada Day
numberAsPOSIXct(cbind(2013, 0), type="yearday") # start of 2013
}

\seealso{\code{\link{numberAsHMS}}}

\author{Dan Kelley}

\keyword{misc}
