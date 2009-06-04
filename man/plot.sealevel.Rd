\name{plot.sealevel}

\alias{plot.sealevel}

\title{Plot sealevel data}

\description{Plot a summary diagram for sealevel data.}

\usage{\method{plot}{sealevel}(x, which=1:4, adorn=NULL,
     draw.time.range=getOption("oce.draw.time.range"),
     mgp=getOption("oce.mgp"), 
     mar=c(mgp[1], mgp[1], par("cex"), par("cex")),
    \dots)}

\arguments{
  \item{x}{an object of class \code{"sealevel"}, e.g. as read by
    \code{\link{read.sealevel}}.}
  \item{which}{numerical vector indicating desired plot types.  
    The value \code{which}=1 gives a time-series of sealevel for the
    entire data set, \code{which}=2 gives sealevel for just the first 28
    days, \code{which}=3 gives a power spectrum of sea level, and
    \code{which}=4 gives a cumulative time integral of that spectrum.}
  \item{adorn}{vector of expressions to be executed for the panels in
    turn, e.g. to adorn the plots.  If the number matches the number of
    panels, then the strings are applied to the appropriate panels,
    as they are drawn from top-left to bottom-right.   If only a single
    expression is provided, it is used for all panels. (See
    \dQuote{Examples}.)}
  \item{draw.time.range}{boolean that applies to panels with time as the
    horizontal axis, indicating whether to draw the time range in 
    the top-left margin of the plot.}
  \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
    also for \code{par(mar)}, computed from this.  The default is
    tighter than the R default, in order to use more space for the data
    and less for the axes.}
  \item{mar}{value to be used with \code{\link{par}("mar")}.}
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{
  Creates a plot for a sea-level dataset, in one of two varieties. 

  Depending on the length of \code{which}, either a single-panel or
  multi-panel plot is drawn.  If there is just one panel, then the
  value of \code{par} used in \code{plot.sealevel} is retained upon
  exit, making it convenient to add to the plot.  For multi-panel
  plots, \code{par} is returned to the value it had before the call,
  and so \code{adorn} must be used to add to individual panels.
}

\value{None.}

\seealso{
  \code{\link{summary.sealevel}} summarizes the information, while
  \code{\link{read.sealevel}} scans it from a file.
}

\examples{
library(oce)
data(sealevel.hal)
# Overall plot
plot(sealevel.hal)

# Illustrate storm surge during Hurricane Juan
plot(sealevel.hal,which=1,xlim=as.POSIXct(c("2003-09-24","2003-10-05"), tz="UTC"))
abline(v=as.POSIXct("2003-09-29 04:00:00", tz="UTC"), col="red")
mtext("Hurricane\nJuan", at=as.POSIXct("2003-09-29 04:00:00", tz="UTC"), col="red")
}

\references{The example refers to Hurricane Juan, which caused a great
  deal of damage to Halifax in 2003.  Since this was in the era of the
  digital photo, a casual web search will uncover some spectacular
  images of damage, from both wind and storm surge.  The wikipedia entry
  \url{http://en.wikipedia.org/wiki/Hurricane_Juan} provides a good
  entry to the topic, with the Canadian Hurricane Centre's
  \url{http://www.atl.ec.gc.ca/weather/hurricane/juan/summary_e.html}
  filling in some more technical details.
}

\author{Dan Kelley}

\keyword{misc}
