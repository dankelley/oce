\name{plot.profile}

\alias{plot.profile}

\title{Plot a CTD profile of various quantities}

\description{Plot a CTD profile, in any of several common formats.}

\usage{plot.profile(x,
             xtype="S", 
             ytype=c("pressure", "z"),
             col.salinity="darkgreen",
             col.temperature="red",
             col.rho="blue",
             col.N2="brown",
             col.dpdt="darkgreen",
             col.time="darkgreen",
             grid=TRUE,
             col.grid="lightgray",
             lty.grid="dotted",
             Slim, Tlim, densitylim, N2lim, dpdtlim, timelim, ylim,
             lwd,
             xaxs="r", yaxs="r",
             cex=1,
             use.smoothScatter=FALSE,
             mgp=getOption("oce.mgp"),
             mar=c(mgp[1]+1, mgp[1]+1, mgp[1] + 1.5, 0.5),
\dots)
}

\arguments{
  \item{x}{A \code{cdt} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{xtype}{Type of profile to plot, from the list below.
    \describe{
      \item{\code{S}}{Profile of salinity.}
      \item{\code{T}}{Profile of \emph{in-situ} temperature.}
      \item{\code{density}}{Profile of density (expressed as
	\eqn{\sigma_\theta}{sigma_theta}).}
      \item{\code{index}}{Index of sample (very useful for working with
	\code{\link{ctd.trim}}).}
      \item{\code{S+T}}{Profile of salinity and temperature within a
	single axis frame.}
      \item{\code{N2}}{Profile of square of buoyancy frequency
	\eqn{N^2}{N^2}, calculated with \code{\link{sw.N2}} with an
	optional argument setting of \code{df=length(x$p)/4} to do some
	smoothing.}
      \item{\code{density+N2}}{Profile of sigma-theta and the square of buoyancy
	frequency within a single axis frame.}
      \item{\code{density+dpdt}}{Profile of sigma-theta and dP/dt for
	the sensor.  The latter is useful in indicating problems with
	the deployment.  It is calculated by first differencing pressure
	and then using a smoothing spline on the result (to avoid
	grid-point wiggles that result because the SBE software only
	writes 3 decimal places in pressure).  Note that dP/dt may be
	off by a scale factor; this should not be a problem if there is
	a \code{x$data$time} column, or a \code{x$metadata$sample.rate}
	value in the header.}
    }
  }
  \item{ytype}{variable to use on y axis.}
  \item{col.salinity}{Color for salinity profile.}
  \item{col.temperature}{Color for temperature.}
  \item{col.rho}{Color for density.}
  \item{col.N2}{Color for square of buoyancy frequency.}
  \item{col.dpdt}{Color for dP/dt.}
  \item{col.time}{Color for delta-time.}
  \item{grid}{logical, set to \code{TRUE} to get a grid.}
  \item{col.grid}{colour for grid.}
  \item{lty.grid}{line type for grid.}
  \item{Slim}{Optional limit for S axis}
  \item{Tlim}{Optional limit for T axis}
  \item{densitylim}{Optional limit for density axis}
  \item{N2lim}{Optional limit for N2 axis}
  \item{dpdtlim}{Optional limit for dp/dt axis}
  \item{timelim}{Optional limit for delta-time axis}
  \item{ylim}{Optional limit for y axis}
  \item{lwd}{lwd value for data line}
  \item{xaxs}{value of \code{\link{par}} \code{xaxs} to use}
  \item{yaxs}{value of \code{\link{par}} \code{yaxs} to use}
  \item{cex}{size of symbols}
  \item{use.smoothScatter}{boolean, set to \code{TRUE} to use 
    \code{\link{smoothScatter}} instead of \code{\link{plot}} to draw
    the plot.}
  \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
    also for \code{par(mar)}, computed from this.  The default is
    tighter than the R default, in order to use more space for the data
    and less for the axes.}
  \item{mar}{value to be used with \code{\link{par}("mar")}.}
  \item{\dots}{Optional arguments passed to other functions.  A common example is
    to set \code{df}, for use in \link{sw.N2} calculations.} 
}

\value{None.}

\seealso{\code{\link{read.ctd}} scans ctd information from a file, and
  \code{\link{plot.TS}} plots a temperature-salinity diagram. 
}

\examples{
library(oce)
data(ctd)
plot.profile(ctd, xtype="T")
}

\author{Dan Kelley}

\keyword{misc}
