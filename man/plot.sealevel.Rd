\name{plot.sealevel}

\alias{plot.sealevel}

\title{Plot sealevel data}

\description{Plot a summary diagram for sealevel data.}

\usage{\method{plot}{sealevel}(x, focus.time=NULL, adorn=NULL, \dots)}

\arguments{
  \item{x}{A \code{sealevel} object, e.g. as read by \code{\link{read.sealevel}}.}
  \item{focus.time}{if provided, the the time interval on which to
    focus, each a string in a format suitable for interpretation by
    \code{\link{as.POSIXct}}; see the example.}
  \item{adorn}{list of character strings containing commands to be
    executed for the panels.  If the number matches the number of
    panels, then the strings are applied to the appropriate panels, as
    they are drawn from top-left to bottom-right.   If only a single
    string is provided, it is used for all panels.}
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{
  Creates a plot for a sea-level dataset, in one of two varieties. 

  If a \code{focus.time} is provided, then the plot is a simple time
  series extending between the two times.

  If a \code{focus.time} is not not provided, then a multi-panel display
  is drawn. The top panel shows the entire timeseries; since this
  typically extends over a year, the graph mainly shows low-frequency
  modulations of the tide, such as spring-neap cycles. The second panel
  focusses on the first month of data, providing a quick visual signal
  as to the nature of the tide, e.g. mainly semidiurnal, mainly diurnal,
  or mixed. In both of these panels, the sealevel is plotted as an
  anomaly in excess of the mean over the whole timeseries. This mean
  value is indicated in the right-hand margin. If the sealevel series
  contains missing values, then only these two panels are
  drawn. However, if it has no missing values, then two spectral
  representations are also drawn. The first of these is a power
  spectrum, with some common tidal constituents being indicates with
  lines and labels. The units are \eqn{m^2}{m^2} per cycle-per-day. The
  second, and thus the lower of the four panels, is a cumulative graph
  of the square root of the power spectrum. The unit for this graph is
  \eqn{m}{m}, so that the step for each tidal constituent may be
  interpreted as the amplitude of that constituent, and the largest
  value on the graph gives the standard deviation of sealevel height.
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
# Focus on 2003-Sep-28 to 29th, the time when Hurricane Juan caused flooding
plot(sealevel.hal, focus.time=c("2003-09-23","2003-10-05"))
abline(v=as.POSIXct("2003-09-28 23:30:00"), col="red", lty="dotted")
mtext("Hurricane\nJuan", at=as.POSIXct("2003-09-28 23:30:00"), col="red")
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
