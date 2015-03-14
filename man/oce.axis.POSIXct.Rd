\name{oce.axis.POSIXct}

\alias{oce.axis.POSIXct}

\title{Modified version of axis.POSIXct}

\description{Modified version of axis.POSIXct}

\usage{oce.axis.POSIXct(side, x, at, tformat, labels = TRUE,
  drawTimeRange=getOption("oceDrawTimeRange"), 
  abbreviateTimeRange=FALSE, drawFrequency=FALSE,
  cex=par("cex"), cex.axis=par("cex.axis"), cex.main=par("cex.main"), 
  mar=par("mar"), mgp=par("mgp"),
  main="", debug=getOption("oceDebug"), ...)}

\arguments{
  \item{side}{as for \code{\link{axis.POSIXct}}.}
  \item{x}{as for \code{\link{axis.POSIXct}}.}
  \item{at}{as for \code{\link{axis.POSIXct}}.}
  \item{tformat}{as \code{format} for \code{\link{axis.POSIXct}} for now, but
      eventually will have new features for multiline labels, e.g. day on one
      line and month on another.}
  \item{labels}{as for \code{\link{axis.POSIXct}}.}
  \item{drawTimeRange}{boolean, \code{TRUE} to draw a time range on
    the opposite side of the plot.}
  \item{drawFrequency}{boolean, \code{TRUE} to show the frequency of
    sampling in the data}
  \item{abbreviateTimeRange}{boolean, \code{TRUE} to abbreviate the
    second number in the time range, e.g. dropping the year if it is the
    same in the first number.}
  \item{cex}{size of labels on axes; see \code{\link[graphics]{par}}("cex").}
  \item{cex.axis}{see \code{\link[graphics]{par}}("cex.axis").}
  \item{cex.main}{see \code{\link[graphics]{par}}("cex.main").}
  \item{mar}{value for \code{par(mar)} for axis}
  \item{mgp}{value for \code{par(mgp)} for axis}
  \item{main}{title of plot}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
    moderate amount of debugging information, or to 2 to get more.} 
  \item{\dots}{as for \code{\link{axis.POSIXct}}.}
}

\details{As \code{\link{axis.POSIXct}} but with axis labels obeying the
  timezone of \code{x}.  This will not be needed for \R 2.9 and later,
  but is included so that \code{oce} will work even with earlier versions.}

\value{A vector of times corresponding to axis ticks is returned silently.}

\seealso{This is used mainly by \code{\link{oce.plot.ts}}.}

\author{Dan Kelley}

\keyword{misc}
