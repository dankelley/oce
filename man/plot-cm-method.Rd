% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cm.R
\name{plot,cm-method}
\alias{plot,cm-method}
\alias{plot.cm}
\title{Plot a cm Object}
\usage{
\S4method{plot}{cm}(
  x,
  which = c(1:2),
  type = "l",
  xlim,
  ylim,
  xaxs = "r",
  yaxs = "r",
  drawTimeRange = getOption("oceDrawTimeRange"),
  drawZeroLine = FALSE,
  mgp = getOption("oceMgp"),
  mar = c(mgp[1] + 1.5, mgp[1] + 1.5, 1.5, 1.5),
  small = 2000,
  main = "",
  tformat,
  debug = getOption("oceDebug"),
  ...
)
}
\arguments{
\item{x}{a \linkS4class{cm} object.}

\item{which}{list of desired plot types.  These are graphed in panels running
down from the top of the page.  See \dQuote{Details} for the meanings of various
values of \code{which}.}

\item{type}{type of plot, as for \code{\link[=plot]{plot()}}.}

\item{xlim, ylim}{optional limit to the x and y axes, passed to \code{\link[=oce.plot.ts]{oce.plot.ts()}} for
time-series plots.}

\item{xaxs, yaxs}{optional controls over the limits of the x and y axes,
passed to \code{\link[=oce.plot.ts]{oce.plot.ts()}} for time-series plots.  These values default
to \code{"r"}, meaning to use the regular method of extend the plot past
its normal limits.  It is common to use \code{"i"} to make the graph extend
to the panel limits.}

\item{drawTimeRange}{boolean that applies to panels with time as the horizontal
axis, indicating whether to draw the time range in the top-left margin of the
plot.}

\item{drawZeroLine}{boolean that indicates whether to draw zero lines on
velocities.}

\item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and also for
\code{par(mar)}, computed from this.  The default is tighter than the R default,
in order to use more space for the data and less for the axes.}

\item{mar}{value to be used with \code{\link{par}}\code{("mar")}.}

\item{small}{an integer indicating the size of data set to be considered
"small", to be plotted with points or lines using the standard
\code{\link[=plot]{plot()}} function.  Data sets with more than \code{small} points will
be plotted with \code{\link[=smoothScatter]{smoothScatter()}} instead.}

\item{main}{main title for plot, used just on the top panel, if there are
several panels.}

\item{tformat}{optional argument passed to \code{\link[=oce.plot.ts]{oce.plot.ts()}}, for plot
types that call that function.  (See \code{\link[=strptime]{strptime()}} for the format
used.)}

\item{debug}{a flag that turns on debugging.  Set to 1 to get a moderate amount
of debugging information, or to 2 to get more.}

\item{...}{Optional arguments passed to plotting functions.}
}
\description{
Creates a multi-panel summary plot of data measured by a current meter.
}
\details{
The panels are controlled by the \code{which} argument, as follows.
\itemize{
\item \code{which=1} or \code{which="u"} for a time-series graph of eastward
velocity, \code{u}, as a function of time.
\item \code{which=2} or \code{which="v"} for a time-series graph of
northward velocity, \code{u}, as a function of time.
\item \code{which=3} or \code{"progressive vector"} for progressive-vector
plot
\item \code{which=4} or \code{"uv"} for a plot of \code{v} versus \code{u}.
(Dots are used for small datasets, and smoothScatter for large ones.)
\item \code{which=5} or \code{"uv+ellipse"} as the \code{"uv"} case, but
with an added indication of the tidal ellipse, calculated from the eigen
vectors of the covariance matrix.
\item \code{which=6} or \code{"uv+ellipse+arrow"} as the \code{"uv+ellipse"}
case, but with an added arrow indicating the mean current.
\item \code{which=7} or \code{"pressure"} for pressure
\item \code{which=8} or \code{"salinity"} for salinity
\item \code{which=9} or \code{"temperature"} for temperature
\item \code{which=10} or \code{"TS"} for a TS diagram
\item \code{which=11} or \code{"conductivity"} for conductivity
\item \code{which=20} or \code{"direction"} for the direction of flow
}
}
\examples{
library(oce)
data(cm)
summary(cm)
plot(cm)

}
\seealso{
Other functions that plot oce data: 
\code{\link{download.amsr}()},
\code{\link{plot,adp-method}},
\code{\link{plot,adv-method}},
\code{\link{plot,amsr-method}},
\code{\link{plot,argo-method}},
\code{\link{plot,bremen-method}},
\code{\link{plot,coastline-method}},
\code{\link{plot,ctd-method}},
\code{\link{plot,gps-method}},
\code{\link{plot,ladp-method}},
\code{\link{plot,landsat-method}},
\code{\link{plot,lisst-method}},
\code{\link{plot,lobo-method}},
\code{\link{plot,met-method}},
\code{\link{plot,odf-method}},
\code{\link{plot,rsk-method}},
\code{\link{plot,satellite-method}},
\code{\link{plot,sealevel-method}},
\code{\link{plot,section-method}},
\code{\link{plot,tidem-method}},
\code{\link{plot,topo-method}},
\code{\link{plot,windrose-method}},
\code{\link{plot,xbt-method}},
\code{\link{plotProfile}()},
\code{\link{plotScan}()},
\code{\link{plotTS}()},
\code{\link{tidem-class}}

Other things related to cm data: 
\code{\link{[[,cm-method}},
\code{\link{[[<-,cm-method}},
\code{\link{applyMagneticDeclination,cm-method}},
\code{\link{as.cm}()},
\code{\link{cm}},
\code{\link{cm-class}},
\code{\link{read.cm}()},
\code{\link{rotateAboutZ}()},
\code{\link{subset,cm-method}},
\code{\link{summary,cm-method}}
}
\author{
Dan Kelley
}
\concept{functions that plot oce data}
\concept{things related to cm data}
