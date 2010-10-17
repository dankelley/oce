\name{window.oce}

\alias{window.oce}

\title{Window an oce object by time}

\description{Window an oce object by time}

\usage{\method{window}{oce}(x, start = NULL, end = NULL,
     frequency = NULL, deltat = NULL, extend = FALSE,
     which=c("time","distance"), index.return=FALSE,
     debug=getOption("oce.debug"), \dots)}

\arguments{
  \item{x}{an \code{oce} object.}
  \item{start}{the start time (or distance) of the time (or space)
    region of interest.  This may be a single value or a vector.}
  \item{end}{the end time (or distance) of the time (or space)
    region of interest.  This may be a single value or a vector.}
  \item{frequency}{not permitted yet.}
  \item{deltat}{not permitted yet}
  \item{extend}{not permitted yet}
  \item{which}{string containing the name of the quantity on which
    sampling is done.  Possibilities are \code{"time"}, which applies
    the windowing on \code{x$data$ts$time}, and \code{"distance"}, which
    applies the windowing on \code{x$data$ss$distance} (for those
    objects, such as \code{adp}, that have a \code{data$ss$distance}
    component).}
  \item{index.return}{boolean flag indicating whether to return a list
    of the "kept" indices for \code{data$ts} and \code{data$ts.slow}
    (either of which may be \code{NULL}, if the object lacks those items.}
  \item{debug}{a flag that turns on debugging.}
  \item{\dots}{ignored}
}

\details{Windows \code{x} on either time or distance, depending on the
  value of \code{which}.  In each case, values of \code{start} and
  \code{end} may be integers, to indicate a portion of the time or
  distance range.  If \code{which} is \code{"time"}, then the
  \code{start} and \code{end} values may also be provided as POSIX
  times, or character strings indicating times (in time zone
  given by the value of \code{getOption("oce.tz")}).}

\value{Normally, this is new \code{oce} object.  However, if
  \code{index.return=TRUE}, the return value is two-element list
  containing items named \code{index} and \code{index.slow}, which are
  the indices for \code{data$ts} and \code{data$ts.slow} that are within
  the window.}

\seealso{\code{\link{subset.oce}} provides more flexible selection of
subsets.}

\examples{
library(oce)
data(adp)
plot(adp)
early <- window(adp, start="2008-06-26 00:00:00", end="2008-06-26 12:00:00")
plot(early)
bottom <- window(adp, start=0, end=20, which="distance")
plot(bottom)
}

\author{Dan Kelley}

\keyword{misc}
