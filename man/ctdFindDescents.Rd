% vim:textwidth=100:expandtab:shiftwidth=2:softtabstop=2
\name{ctdFindDescents}

\alias{ctdFindDescents}

\title{Find descending portions of a CTD towyow}

\description{Examine the pressure time series to identify intervals times during which a towyowing
  CTD instrument was descending.  Then either return pointers to the start/end times of the
  descents, or a vector of CTD objects holding those descents.}

\usage{ctdFindDescents(x, dpCriterion=0.3, k=21, smallest=10, method=3,
                arr.ind=FALSE, debug=getOption("oceDebug"))}

\arguments{
  \item{x}{A \code{ctd} object, as read by \code{\link{read.ctd}} or created by
    \code{\link{as.ctd}}.}
  \item{dpCriterion}{a quantile criterion for \code{diff(pressure)}}
  \item{k}{\code{k} used in \code{\link{runmed}} for \code{method=2}}
  \item{smallest}{shortest descent candidates that are kept}
  \item{method}{1 for a simple method, 2 for a method using \code{\link{runmed}}, or 3 for a method using \code{\link{smooth}}.}
  \item{arr.ind}{should array indices be returned, or a vector of ctd objects?}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a moderate amount of debugging information, or to 2 to get more.} 

}

\details{All methods start by computing \code{diff(pressure)}.  With \code{method=1}, negative
  differences are ignored, and then differences with \code{\link{quantile}} exceeding
  \code{dpCriterion} are considered to be within a descending portion.  This tends to produce many
  descents even within ascents, making \code{method=1} unreliable.

  With \code{method=2}, the pressure differences are run through a running median filter (using
  \code{\link{runmed}} with \code{k} as provided in the argument), and the result is subjected to
  the quantile treatment.  This works better than \code{method=1}, in test cases.

  With \code{method=3}, the pressure differences are run through a smoother (with
  \code{\link{smooth}}), and then the quantile treatment is used.  This works well in test cases,
  subdividing the data as one would by eye.

  It is important to set \code{smallest} appropriately, to avoid detecting small descents within an
  ascending portion.

}

\value{If \code{arr.ind=TRUE}, a data frame with columns \code{start} and \code{end}, the indices of
  the downcasts.  Otherwise, a vector of \code{ctd} objects.}

\section{status}{This function was developed in late July 2013, and needs more testing before it
  will be complete.}

\seealso{The documentation for \code{\link{ctd-class}} explains the structure
    of CTD objects, and also outlines the other functions dealing with them.}

\examples{
\dontrun{
library(oce)
d <- read.csv("towyow.csv", header=TRUE)
towyow <- as.ctd(d$salinity, d$temperature, d$pressure)

casts <- ctdFindDescents(towyow)
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
