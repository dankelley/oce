\name{plot.lobo}

\alias{plot.lobo}
\alias{plot,lobo,missing-method}
\alias{plot,lobo-method}

\title{Plot lobo data}

\description{Plot a summary diagram for lobo data.}

\usage{\S4method{plot}{lobo}(x, 
     which=c(1,2,3), 
     adorn=NULL,
     mgp=getOption("oceMgp"), 
     mar=c(mgp[2]+1, mgp[1]+1, 1, mgp[1]+1.25),
     debug=getOption("oceDebug"),
     \dots)}

\arguments{
  \item{x}{A \code{lobo} object, e.g. as read by \code{\link{read.lobo}}.}

  \item{which}{A vector of numbers or character strings, indicating the
      quantities to plot.  These are stacked in a single column.  The possible
      values for \code{which} are as follows: \code{1} or \code{"temperature"}
      for a time series of temperature; \code{2} or \code{"salinity"} for
      salinity; \code{3} or \code{"TS"} for a TS diagram; \code{4} or
      \code{"u"} for a timeseries of the u component of velocity; \code{5} or
      \code{"v"} for a timeseries of the v component of velocity; \code{6} or
      \code{"nitrate"} for a timeseries of nitrate concentration; \code{7} or
      \code{"fluorescence"} for a timeseries of fluorescence value.}

  \item{adorn}{list of expressions to be executed for the panels in
    turn, e.g. to adorn the plots.  If the number matches the number 
    of panels, then the strings are applied to the appropriate panels,
    as they are drawn from top-left to bottom-right.   If only a single
    expression is provided, it is used for all panels. (See
    \dQuote{Examples}.)}
  \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
    also for \code{par(mar)}, computed from this.  The default is
    tighter than the R default, in order to use more space for the data
    and less for the axes.}
  \item{mar}{value to be used with \code{\link{par}("mar")}.}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
    moderate amount of debugging information, or to 2 to get more.} 
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Creates a summary plot for a lobo dataset.}

\value{None.}

\seealso{The documentation for \code{\link{lobo-class}} explains the structure
    of LOBO objects, and also outlines the other functions dealing with them.}

\examples{
library(oce)
data(lobo)
plot(lobo)
# illustrate adornment
plot(lobo,
     adorn=expression({abline(v=as.POSIXct("2009-03-23 00:00:00"), col="red")}))
}
\references{\url{http://lobo.satlantic.com/} and
\url{http://www.mbari.org/lobo/}
}

\author{Dan Kelley}

\keyword{hplot}
