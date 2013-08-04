% vim:textwidth=100:expandtab:shiftwidth=2:softtabstop=2
\name{ctdFindProfiles}

\alias{ctdFindProfiles}

\title{Find profiles within a towyow CTD record}

\description{Examine the pressure record looking for extended periods of either ascent or descent,
  and return either indices to these events or a vector of CTD records containing the events.}

\usage{ctdFindProfiles(x, cutoff=0.5, smallest=10, arr.ind=FALSE,
 direction=c("descending", "ascending"), debug=getOption("oceDebug"), ...)}

\arguments{
  \item{x}{A \code{ctd} object, as read by \code{\link{read.ctd}} or created by
    \code{\link{as.ctd}}.}
  \item{cutoff}{criterion on pressure difference; see \dQuote{Details}.}
  \item{smallest}{lower limit on number of points in candidate profiles}
  \item{arr.ind}{should array indices be returned, or a vector of ctd objects?}
  \item{direction}{string indicating the travel direction to be selected}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a moderate amount of debugging
    information, or to 2 to get more.} 
  \item{\dots}{extra arguments that are passed to \code{\link{smooth.spline}}.}
}

\details{The method works by examining the pressure record.  First, this is smoothed using
  \code{\link{smooth.spline}}, which is provided with any extra arguments as supplied to the present
  function, e.g. \code{ctdFindProfiles(..., df=10)} uses a spline with 10 degrees of freedom.  The
  spline is then first differenced with \code{\link{diff}}.  Median values of the positive and
  negative first-difference values are then multiplied by \code{cutoff}.  This establishes criteria
  for any given point to be in an ascending profile, a descending profile, or a non-profile.
  Contiguous regions are then found, and those that have more than \code{smallest} points are
  discarded.}

\value{If \code{arr.ind=TRUE}, a data frame with columns \code{start} and \code{end}, the indices of
  the downcasts.  Otherwise, a vector of \code{ctd} objects.}

\seealso{The documentation for \code{\link{ctd-class}} explains the structure
    of CTD objects, and also outlines the other functions dealing with them.}

\examples{
\dontrun{
library(oce)
d <- read.csv("towyow.csv", header=TRUE)
towyow <- as.ctd(d$salinity, d$temperature, d$pressure)

casts <- ctdFindProfiles(towyow)
par(mfrow=c(length(casts), 3))
for (cast in casts) {
  plotProfile(cast, "salinity")
  plotProfile(cast, "temperature")
  plotTS(cast, type='o')
}
}
}

\author{Dan Kelley}

\keyword{misc}
