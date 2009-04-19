\name{plot.tidem}

\alias{plot.tidem}

\title{Plot a tidal fit}

\description{Plot a summary diagram for a tidal fit.}

\usage{\method{plot}{tidem}(x, label.if=NULL, plot.type=c("staircase", "spikes"),
    log="",   mgp=getOption("oce.mgp"), ...)}

\arguments{
  \item{x}{a \code{tidem} object, as created by \code{\link{tidem}}.}
  \item{label.if}{if NULL, the function will indicate some particular
	tidal constituents; if a value is provided, labels will be given for
	any constituent with amplitude exceeding the value provided.}
  \item{plot.type}{the type for the plot.  The \code{staircase} style
	shows a cumulative representation, in which the heights of the rising
	portions indicates the energy in the constituents.  The \code{spike}
	style is in the more conventional spectral form.  If a large number of
	constituents is being used, the \code{staircase} style can be simpler
	to read at a glance.} 
  \item{log}{if set to "\code{x}", the frequency axis will be
	logarithmic.}
  \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
    also for \code{par(mar)}, computed from this.  The default is
    tighter than the R default, in order to use more space for the data
    and less for the axes.}
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Creates a plot for a "\code{tidem}" object.  See
  \code{\link{tidem}} for examples.}

\seealso{\code{\link{tidem}} fits a "\code{tidem}" object, and
\code{\link{summary.tidem}} summarizes such an object.
}

\examples{
library(oce)
data(sealevel.hal)
tide <- tidem(sealevel.hal)
plot(tide)
}

\author{Dan Kelley}
\keyword{misc}
