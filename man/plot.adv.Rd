\name{plot.adv}

\alias{plot.adv}

\title{Plot ADV data}

\description{Plot ADV data.}

\usage{\method{plot}{adv}(x,
     which=1:3,
     titles,
     adorn=NULL,
     draw.time.range=getOption("oce.draw.time.range"),
     mgp=getOption("oce.mgp"),
     mar=c(mgp[1],mgp[1]+1,1,1/4),
     margins.as.image=FALSE,
     cex=1,
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
  \item{adorn}{list of expressions to be executed for the panels in
    turn, e.g. to adorn the plots.  If the number matches the number of
    panels, then the strings are applied to the appropriate panels, 
    as they are drawn from top-left to bottom-right.   If only a single
    expression is provided, it is used for all panels.  (See
    \dQuote{Examples}.)}
  \item{draw.time.range}{boolean that applies to panels with time as the
    horizontal axis, indicating whether to draw the time range in 
    the top-left margin of the plot.}
  \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
    also for \code{par(mar)}, computed from this.  The default is
    tighter than the R default, in order to use more space for the data
    and less for the axes.}
  \item{mar}{value to be used with \code{\link{par}("mar")}.}
  \item{margins.as.image}{boolean, \code{TRUE} to put a wide margin to the
    right of time-series plots, matching the space used up by a palette
    in an \code{\link{imagep}} plot.}
  \item{cex}{value for the character-expansion parameter, used by \code{\link{par}("cex")}.}
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Creates a multi-panel summary plot of data measured by an ADV.
  The panels are controlled by the \code{which} argument.

  \itemize{ 
    \item \code{which=1} yields a timeseries of the x component of velocity.
    \item \code{which=2} yields a timeseries of the y component of velocity.
    \item \code{which=3} yields a timeseries of the z component of velocity.
    \item \code{which=14} yields a timeseries of temperature.
    \item \code{which=15} yields a timeseries of pressure.
  }
}

\seealso{See \code{\link{read.adv}} for notes on functions relating to
  \code{"adv"} objects.}

\examples{
\dontrun{
library(oce)
adv <- read.adv("~/data/sleiwex-archive/2008/mooring/m6/autopod/day183/SL08A183.015", 
       sampling.start=as.POSIXct("2008-07-01 15:00:00", tz="UTC"), deltat=0.1)
plot(adv)
}
}

\author{Dan Kelley}
\keyword{misc}



