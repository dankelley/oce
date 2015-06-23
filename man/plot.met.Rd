\name{plot.met}

\alias{plot.met}
\alias{plot,met,missing-method}
\alias{plot,met-method}

\title{Plot meteorological data}

\description{Plot meteorological data}


\usage{\S4method{plot}{met}(x,
     which=1:4, 
     mgp=getOption("oceMgp"),
     mar=c(mgp[1]+1,mgp[1]+1,mgp[1]+1,mgp[1]+1),
     tformat,
     debug=getOption("oceDebug"),
     \dots)
}
\arguments{
  \item{x}{A \code{ctd} object, e.g. as read by \code{\link{read.met}}, or a
      list containing items named \code{salinity} and \code{temperature}.}
  \item{which}{list of desired plot types.  
    \itemize{
      \item \code{which=1} gives a time-series plot of temperature
      \item \code{which=2} gives a time-series plot of pressure 
      \item \code{which=3} gives a time-series plot of the x (eastward) component of velocity
      \item \code{which=4} gives a time-series plot of the y (northward) component of velocity
    }
  }
  \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
    also for \code{par(mar)}, computed from this.  The default is
    tighter than the R default, in order to use more space for the data
    and less for the axes.}
  \item{mar}{value to be used with \code{\link{par}("mar")}.}
  \item{tformat}{optional argument passed to \code{\link{oce.plot.ts}}, for
      plot types that call that function.  (See \code{\link{strptime}} for the
      format used.)}
  \item{debug}{set to \code{TRUE} to get debugging information during processing.}
  \item{\dots}{optional arguments passed to plotting functions. A common
    example is to set \code{df}, for use in \link{swN2} calculations.} 
}

\details{Creates a multi-panel summary plot of data measured in a
    meteorological data set.  cast. The panels are controlled by the
    \code{which} argument.  Normally, 4 panels are specified with the
    \code{which}, but it can also be useful to specify less than 4 panels, and
    then to draw other panels after this call.

  If more than one panel is drawn, then on exit from \code{plot.met},
  the value of \code{par} will be reset to the value it had before the
  function call.  However, if only one panel is drawn, the adjustments
  to \code{par} made within \code{plot.met} are left in place, so that
  further additions may be made to the plot.
}

\seealso{The documentation for \code{\link{met-class}} explains the structure
    of \code{met} objects, and also outlines the other functions dealing with
    them.}

\examples{
library(oce)
data(met)
plot(met, which=3:4)
}

\author{Dan Kelley}

\keyword{hplot}
