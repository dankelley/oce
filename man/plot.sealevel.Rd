\name{plot.sealevel}

\alias{plot.sealevel}
\alias{plot,sealevel,missing-method}
\alias{plot,sealevel-method}

\title{Plot sealevel data}

\description{Plot a summary diagram for sealevel data.}

\usage{\S4method{plot}{sealevel}(x, which=1:3, adorn=NULL,
drawTimeRange=getOption("oceDrawTimeRange"),
mgp=getOption("oceMgp"), 
mar=c(mgp[1]+0.5,mgp[1]+1.5,mgp[2]+1,mgp[2]+3/4),
marginsAsImage=FALSE,
debug=getOption("oceDebug"),
\dots)}

\arguments{
    \item{x}{an object of class \code{"sealevel"}, e.g. as read by
        \code{\link{read.sealevel}}.}

    \item{which}{a numerical or string vector indicating desired plot types, with
        possibilities 1 or \code{"all"} for a time-series of all the data, 2 or
        \code{"month"} for a time-series of just the first month, 3 or
        \code{"spectrum"} for a power spectrum (truncated to frequencies below
        0.1 cycles per hour, or 4 or \code{"cumulativespectrum"} for a
        cumulative integral of the power spectrum.}

    \item{adorn}{vector of expressions to be executed for the panels in
        turn, e.g. to adorn the plots.  If the number matches the number of
        panels, then the strings are applied to the appropriate panels,
        as they are drawn from top-left to bottom-right.   If only a single
        expression is provided, it is used for all panels. (See
        \dQuote{Examples}.)}

    \item{drawTimeRange}{boolean that applies to panels with time as the
        horizontal axis, indicating whether to draw the time range in 
        the top-left margin of the plot.}

    \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
        also for \code{par(mar)}, computed from this.  The default is
        tighter than the R default, in order to use more space for the data
        and less for the axes.}

    \item{mar}{value to be used with \code{\link{par}("mar")}.}

    \item{marginsAsImage}{boolean, \code{TRUE} to put a wide margin to the
        right of time-series plots, matching the space used up by a palette
        in an \code{\link{imagep}} plot.}

    \item{debug}{a flag that turns on debugging, if it exceeds 0.}

    \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Creates a plot for a sea-level dataset, in one of two varieties.
    Depending on the length of \code{which}, either a single-panel or
    multi-panel plot is drawn.  If there is just one panel, then the value of
    \code{par} used in \code{plot.sealevel} is retained upon exit, making it
    convenient to add to the plot.  For multi-panel plots, \code{par} is
    returned to the value it had before the call, and so \code{adorn} must be
    used to add to individual panels.  }

\value{None.}

\seealso{The documentation for \code{\link{sealevel-class}} explains the
    structure of sealevel objects, and also outlines the other functions
    dealing with them.

}

\examples{
library(oce)
data(sealevel)
## local Halifax time is UTC + 4h; see [2] on timing
juan <- as.POSIXct("2003-09-29 00:15:00", tz="UTC")+4*3600
plot(sealevel, which=1, xlim=juan+86400*c(-7,7))
abline(v=juan, col='red')
}

\references{The example refers to Hurricane Juan, which caused a great deal of
    damage to Halifax in 2003.  Since this was in the era of the digital photo,
    a casual web search will uncover some spectacular images of damage, from
    both wind and storm surge. A map of the path of Hurricane Juan across Nova
    Scotia is at
    \url{http://ec.gc.ca/ouragans-hurricanes/default.asp?lang=En&n=222F51F7-1}
    (link checked April 15, 2015). Landfall, very near the site of this
    sealevel gauge, was between 00:10 and 00:20 Halifax local time on Monday,
    Sept 29, 2003.
}

\author{Dan Kelley}

\keyword{hplot}

