\name{plot.logger}

\alias{plot.logger}
\alias{plot,logger,missing-method}
\alias{plot,logger-method}

\title{Plot logger data}

\description{Plot logger data}

\usage{\S4method{plot}{logger}(x,
     which=c(1,3,4), title="", adorn=NULL,
     tlim, plim, Tlim, 
     xlab, ylab,
     tformat,
     drawTimeRange=getOption("oceDrawTimeRange"),
     abbreviateTimeRange=getOption("oceAbbreviateTimeRange"),
     useSmoothScatter=FALSE,
     mgp=getOption("oceMgp"),
     mar=c(mgp[1]+1.5,mgp[1]+1.5,1.5,1.5),
     main="",
     debug=getOption("oceDebug"),
     \dots)}

\arguments{
  \item{x}{\code{logger} object, typically result of \code{\link{read.logger}}.}
  \item{which}{list of desired plot types.  These are graphed in
    panels running down from the top of the page.  See
    \dQuote{Details} for the meanings of various values of
    \code{which}.}
  \item{title}{character string to be used in the text-summary panel
    (\code{which}=2)}
  \item{adorn}{list of expressions to be executed for the panels in
    turn, e.g. to adorn the plots.  If the number matches the number
    of panels, then the strings are applied to the appropriate panels,
    as they are drawn from top-left to bottom-right.   If only a single
    expression is provided, it is used for all panels.  (See
    \dQuote{Examples}.)}
  \item{tlim}{optional limits for time axis.  If not provided,
    the value will be inferred from the data.}
  \item{plim}{optional limits for pressure axis.  If not provided,
    the value will be inferred from the data.  (It is helpful to specify
    this, if the auto-scaled value will be inappropriate, e.g. if
    more lines are to be added later.)}
  \item{Tlim}{optional limits for temperature axis.  If not provided,
    the value will be inferred from the data.  (It is helpful to specify
    this, if the auto-scaled value will be inappropriate, e.g. if
    more lines are to be added later.)}
  \item{xlab}{optional label for x axis.}
  \item{ylab}{optional label for y axis.}
  \item{tformat}{optional argument passed to \code{\link{oce.plot.ts}}, for
      plot types that call that function.  (See \code{\link{strptime}} for the
      format used.)}
  \item{drawTimeRange}{boolean that applies to panels with time as the
    horizontal axis, indicating whether to draw the time range in 
    the top-left margin of the plot.}
  \item{abbreviateTimeRange}{boolean that applies to panels with time
    as the horizontal axis, indicating whether to abbreviate the second
    time in the time range (e.g. skipping the year, month, day, etc. if
    it's the same as the start time).}
  \item{useSmoothScatter}{a boolean to cause
    \code{\link{smoothScatter}} to be used for profile plots, instead of
    \code{\link{plot}}.}
  \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
    also for \code{par(mar)}, computed from this.  The default is
    tighter than the R default, in order to use more space for the data
    and less for the axes.}
  \item{mar}{value to be used with \code{\link{par}("mar")}.}
  \item{main}{main title for plot, used just on the top panel, if there are several panels.}
  \item{debug}{a flag that turns on debugging, if it exceeds 0.}
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Several plots are available.
  \itemize{
    \item \code{which=1} or \code{"temperature"} for a time-series plot of
    temperature
    \item \code{which=2} or \code{"text"} for textual information about
    the dataset
    \item \code{which=3} or \code{"pressure"} for a time-series plot of pressure
    \item \code{which=4} or \code{"profile"} for a temperature ``profile''
  }
}

\examples{
library(oce)
data(logger)
plot(logger, which=c(1,3))
}

\seealso{The documentation for \code{\link{logger-class}} explains the
    structure of \code{logger} objects, and also outlines the other functions
    dealing with them.}

\author{Dan Kelley}

\keyword{hplot}
