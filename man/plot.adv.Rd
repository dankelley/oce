\name{plot.adv}

\alias{plot.adv}

\title{Plot ADV data}

\description{Plot ADV data.}

\usage{\method{plot}{adv}(x,
     which=c(1:3,14,15),
     titles,
     adorn=NULL,
     draw.time.range=getOption("oce.draw.time.range"),
     draw.zero.line=FALSE,
     mgp=getOption("oce.mgp"),
     mar=c(mgp[1],mgp[1]+1,1,1.5),
     margins.as.image=FALSE,
     cex=par("cex"), cex.axis=par("cex.axis"), cex.main=par("cex.main"),
     xlim,
     ylim,
     debug=getOption("oce.debug"),
     \dots)}

\arguments{
  \item{x}{an \code{adv} object, e.g. as read by \code{\link{read.adv}}.}
  \item{which}{list of desired plot types.  These are graphed in
    panels running down from the top of the page.  See
    \dQuote{Details} for the meanings of various values of
    \code{which}.}
  \item{titles}{optional vector of character strings to be used as
    labels for the plot panels.  For images, these strings will be
    placed in the right hand side of the top margin.  For timeseries,
    these strings are ignored.
    If this is provided, its length must equal that of \code{which}.}
  \item{adorn}{optional list of \code{\link{expression}}s to be performed
    immediately after drawing the panels. (See \code{\link{plot.adp}} for
    an example.)}
  \item{draw.time.range}{boolean that applies to panels with time as the
    horizontal axis, indicating whether to draw the time range in 
    the top-left margin of the plot.}
  \item{draw.zero.line}{boolean that indicates whether to draw zero
    lines on velocities.}
  \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
    also for \code{par(mar)}, computed from this.  The default is
    tighter than the R default, in order to use more space for the data
    and less for the axes.}
  \item{mar}{value to be used with \code{\link{par}("mar")}.}
  \item{margins.as.image}{boolean, \code{TRUE} to put a wide margin to the
    right of time-series plots, matching the space used up by a palette
    in an \code{\link{imagep}} plot.}
  \item{cex}{size of labels on axes; see \code{\link[graphics]{par}}("cex").}
  \item{cex.axis}{see \code{\link[graphics]{par}}("cex.axis").}
  \item{cex.main}{see \code{\link[graphics]{par}}("cex.main").}
  \item{xlim}{optional 2-element list for \code{xlim}, or 2-column matrix, in which case
    the rows are used, in order, for the panels of the graph.}
  \item{ylim}{optional 2-element list for \code{ylim}, or 2-column matrix, in which case
    the rows are used, in order, for the panels of the graph.}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
    moderate amount of debugging information, or to 2 to get more.} 
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Creates a multi-panel summary plot of data measured by an ADV.
  The panels are controlled by the \code{which} argument.  (Note the
  gaps in the sequence, e.g. 4 and 8 are not used.)

  \itemize{ 
    \item \code{which=1} yields a timeseries of the x component of velocity.
    \item \code{which=2} yields a timeseries of the y component of velocity.
    \item \code{which=3} yields a timeseries of the z component of velocity.
    \item \code{which=5:7} yields a timeseries of backscatter intensity
    for beams 1, 2 and 3
    \item \code{which=9:11} yields a timeseries of correlation
    for beams 1, 2 and 3
    \item \code{which=14} yields a timeseries of temperature.
    \item \code{which=15} yields a timeseries of pressure.
    \item \code{which=16} yields a timeseries of heading.
    \item \code{which=17} yields a timeseries of pitch.
    \item \code{which=18} yields a timeseries of roll.
    \item \code{which=19} yields a plot of beam 1 correlation vs
    amplitude, using \code{\link{smoothScatter}}.
    \item \code{which=20} as for \code{which=19} but for beam 2.
    \item \code{which=21} as for \code{which=19} but for beam 3.
    \item \code{which=23} yields a progressive-vector diagram in the
      horizontal plane, plotted with \code{asp=1}, and taking
      beam1 and beam2 as the eastward and northward components of
      velocity, respectively.
  }
}

\seealso{See \code{\link{read.adv}} for notes on functions relating to
  \code{"adv"} objects.}

\examples{
\dontrun{
library(oce)
adv <- read.oce("/data/archive/sleiwex/2008/moorings/m03/adv/sontek_b373h/raw/adv_sontek_b373h.adr",
               from=as.POSIXct("2008-07-01"),
               to=as.POSIXct("2008-07-02"),
               by="00:05:00")
plot(adv)
}
}

\author{Dan Kelley}
\keyword{misc}



