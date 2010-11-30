\name{oce.axis.POSIXct}

\alias{oce.axis.POSIXct}

\title{Modified version of axis.POSIXct}

\description{Modified version of axis.POSIXct}

\usage{oce.axis.POSIXct(side, x, at, format, labels = TRUE,
  draw.time.range=TRUE, abbreviate.time.range=FALSE, draw.frequency=FALSE,
  cex=par("cex"),
  cex.axis=par("cex.axis"),
  cex.main=par("cex.main"), 
  main="", debug=getOption("oce.debug"), ...)}

\arguments{
  \item{side}{as for \code{\link{axis.POSIXct}}.}
  \item{x}{as for \code{\link{axis.POSIXct}}.}
  \item{at}{as for \code{\link{axis.POSIXct}}.}
  \item{format}{as for \code{\link{axis.POSIXct}}.}
  \item{labels}{as for \code{\link{axis.POSIXct}}.}
  \item{draw.time.range}{boolean, \code{TRUE} to draw a time range on
    the opposite side of the plot.}
  \item{draw.frequency}{boolean, \code{TRUE} to show the frequency of
    sampling in the data}
  \item{abbreviate.time.range}{boolean, \code{TRUE} to abbreviate the
    second number in the time range, e.g. dropping the year if it is the
    same in the first number.}
  \item{cex}{size of labels on axes; see \code{\link[graphics]{par}}("cex").}
  \item{cex.axis}{see \code{\link[graphics]{par}}("cex.axis").}
  \item{cex.main}{see \code{\link[graphics]{par}}("cex.main").}
  \item{main}{title of plot.}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
    moderate amount of debugging information, or to 2 to get more.} 
  \item{\dots}{as for \code{\link{axis.POSIXct}}.}
}

\details{As \code{\link{axis.POSIXct}} but with axis labels obeying the
  timezone of \code{x}.  This will not be needed for \R 2.9 and later,
  but is included so that \code{oce} will work even with earlier versions.}

\value{The locations on the axis scale at which tick marks were drawn.}

\seealso{This is used mainly by \code{\link{oce.plot.ts}}.}

\author{Dan Kelley}

\keyword{misc}
