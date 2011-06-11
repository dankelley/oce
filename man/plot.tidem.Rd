\name{plot.tidem}

\alias{plot.tidem}

\title{Plot a tidal fit}

\description{Plot a summary diagram for a tidal fit.}

\usage{\method{plot}{tidem}(x, 
     which=1,
     label.if=NULL, 
     log="",
     mgp=getOption("oceMgp"), 
     mar=c(mgp[1]+1,mgp[1]+1,mgp[2]+0.25,mgp[2]+1),
     ...)}

\arguments{
  \item{x}{a \code{tidem} object, as created by \code{\link{tidem}}.}
  \item{which}{integer flag indicating plot type, 1 for stair-case
    spectral, 2 for spike spectral.}
  \item{label.if}{if NULL, the function will indicate some particular
	tidal constituents; if a value is provided, labels will be given for
	any constituent with amplitude exceeding the value provided.}
  \item{log}{if set to "\code{x}", the frequency axis will be
	logarithmic.}
  \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
    also for \code{par(mar)}, computed from this.  The default is
    tighter than the R default, in order to use more space for the data
    and less for the axes.}
  \item{mar}{value to be used with \code{\link{par}("mar")}.}
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Creates a plot for a "\code{tidem}" object.  See
  \code{\link{tidem}} for examples.}

\seealso{\code{\link{tidem}} fits a "\code{tidem}" object, and
\code{\link{summary.tidem}} summarizes such an object.
}

\examples{
library(oce)
data(sealevelHalifax)
tide <- tidem(sealevelHalifax)
plot(tide)
}

\author{Dan Kelley}
\keyword{hplot}
