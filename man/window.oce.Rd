\name{window.oce}

\alias{window.oce}

\title{Window an oce object by time}

\description{Window an oce object by time}


\usage{\method{window}{oce}(x, start = NULL, end = NULL,
     frequency = NULL, deltat = NULL, extend = FALSE, \dots)}

\arguments{
  \item{x}{an \code{oce} object.}
  \item{start}{start the start time of the period of interest.}
  \item{end}{end the start time of the period of interest.}
  \item{frequency}{not permitted yet.}
  \item{deltat}{not permitted yet}
  \item{extend}{not permitted yet}
  \item{\dots}{ignored.}
}

\details{Based on the generic \code{\link{window}}, this function
  subsets an \code{oce} object based on time.  If the object does not
  contain a time vector (stored in \code{data$ta$time}), a warning is
  given and a copy of the object is returned..

  The start and end times may be integers, referring to sample number,
  or POSIX times, or character strings that can be converted to POSIX
  times.  (In the third case, the timezone is set to
  \code{getOption("oce.tz")}.)
}

\value{A new \code{oce} object.}

\seealso{\code{\link{subset.oce}} provides more flexible selection,
  based on any property, not just time.}

\examples{
library(oce)
# CTD data
data(adp)
plot(adp)
adp.windowed <- window(adp, start=as.POSIXct("2008-06-26 00:00:00",
tz="UTC"), end=as.POSIXct("2008-06-26 12:00:00", tz="UTC"))
plot(adp.windowed)
}

\author{Dan Kelley}

\keyword{misc}
