% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{plotProfile}

\alias{plotProfile}

\title{Plot a profile, with decreasing pressure on the y axis}

\description{Plot a profile, showing variation of some quantity (or quantities)
  with pressure, using the oceanographic convention of putting lower pressures
  nearer the top of the plot. This works for any \code{oce} object that has a
  pressure column in its \code{data} slot.}

\usage{plotProfile(x,
    xtype="salinity+temperature", 
    ytype=c("pressure", "z", "depth", "sigmaTheta"),
    eos=getOption("oceEOS", default="gsw"),
    xlab=NULL, ylab=NULL,
    lty=1, 
    col="black",
    col.salinity="darkgreen",
    col.temperature="red",
    col.rho="blue",
    col.N2="brown",
    col.dpdt="darkgreen",
    col.time="darkgreen",
    pt.bg="transparent",
    grid=TRUE,
    col.grid="lightgray",
    lty.grid="dotted",
    Slim, Clim, Tlim, densitylim, N2lim, Rrholim, dpdtlim, timelim, plim, ylim,
    lwd=par("lwd"),
    xaxs="r", yaxs="r",
    cex=1, pch=1,
    useSmoothScatter=FALSE,
    df,
    keepNA=FALSE,
    type='l',
    mgp=getOption("oceMgp"),
    mar=c(1 + if (length(grep('\\\\+', xtype))) mgp[1] else 0,
          mgp[1]+1.5, mgp[1]+1.5, mgp[1]),
    add=FALSE, inset=FALSE,
    debug=getOption("oceDebug"),
    \dots)
}

\arguments{
  \item{x}{A \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{xtype}{Item(s) plotted on the x axis, either a vector of length equal
    to that of \code{x@data$pressure} or a text code from the list below.
    \describe{
      \item{\code{"salinity"}}{Profile of salinity.}
      \item{\code{"conductivity"}}{Profile of conductivity.}
      \item{\code{"temperature"}}{Profile of \emph{in-situ} temperature.}
      \item{\code{"theta"}}{Profile of \emph{potential} temperature.}
      \item{\code{"density"}}{Profile of density (expressed as
        \eqn{\sigma_\theta}{sigma_theta}).}
      \item{\code{"index"}}{Index of sample (very useful for working with
        \code{\link{ctdTrim}}).}
      \item{\code{"salinity+temperature"}}{Profile of salinity and temperature
        within a single axis frame.}
      \item{\code{"N2"}}{Profile of square of buoyancy frequency \eqn{N^2}{N^2},
        calculated with \code{\link{swN2}} with an optional argument setting of
        \code{df=length(x[["pressure"]])/4} to do some smoothing.}
      \item{\code{"density+N2"}}{Profile of sigma-theta and the square of
        buoyancy frequency within a single axis frame.}
      \item{\code{"density+dpdt"}}{Profile of sigma-theta and dP/dt for the
        sensor.  The latter is useful in indicating problems with the
        deployment.  It is calculated by first differencing pressure and then
        using a smoothing spline on the result (to avoid grid-point wiggles that
        result because the SBE software only writes 3 decimal places in
        pressure).  Note that dP/dt may be off by a scale factor; this should
        not be a problem if there is a \code{time} column in the \code{data}
        slot, or a \code{sample.rate} in the \code{metadata} slot. }
      \item{\code{"spice"}}{Profile of spice}
      \item{\code{"Rrho"}}{Profile of Rrho, defined in the diffusive sense}
      \item{\code{"RrhoSF"}}{Profile of Rrho, defined in the salt-finger sense}
      \item{an expression}{an expression to be evaluated, in the calling
        environment, for some quantity; in this case, it makes sense to
        specify also \code{xlab}.}
    }
  }
  \item{ytype}{variable to use on y axis; note that \code{z} is the negative
    of \code{depth}.}
  \item{eos}{equation of state to be used, either \code{"unesco"} or
    \code{"gsw"}.}
  \item{xlab}{optional label for x axis (at top of plot).}
  \item{ylab}{optional label for y axis.  Set to \code{""} to prevent labelling
    the axis.}
  \item{lty}{line type for the profile.}
  \item{col}{colour for a general profile.}
  \item{col.salinity}{color for salinity profile (see \dQuote{Details}).}
  \item{col.temperature}{color for temperature (see \dQuote{Details}).}
  \item{col.rho}{color for density (see \dQuote{Details}).}
  \item{col.N2}{color for square of buoyancy frequency (see \dQuote{Details}).}
  \item{col.dpdt}{color for dP/dt.}
  \item{col.time}{color for delta-time.}
  \item{pt.bg}{inside color for symbols with \code{pch} in 21:25}
  \item{grid}{logical, set to \code{TRUE} to get a grid.}
  \item{col.grid}{colour for grid.}
  \item{lty.grid}{line type for grid.}
  \item{Slim}{Optional limit for S axis}
  \item{Clim}{Optional limit for conductivity axis}
  \item{Tlim}{Optional limit for T axis}
  \item{densitylim}{Optional limit for density axis}
  \item{N2lim}{Optional limit for N2 axis}
  \item{Rrholim}{Optional limit for Rrho axis}
  \item{dpdtlim}{Optional limit for dp/dt axis}
  \item{timelim}{Optional limit for delta-time axis}
  \item{plim}{Optional limit for pressure axis, ignored unless
    \code{ytype=="pressure"}, in which case it takes precedence over
    \code{ylim}.}
  \item{ylim}{Optional limit for y axis, which can apply to any plot type,
    although is overridden by \code{plim} if \code{ytype=="pressure"}.}
  \item{lwd}{lwd value for data line}
  \item{xaxs}{value of \code{\link{par}} \code{xaxs} to use}
  \item{yaxs}{value of \code{\link{par}} \code{yaxs} to use}
  \item{cex}{size to be used for plot symbols (see \code{\link{par}})}
  \item{pch}{code for plotting symbol (see \code{\link{par}}).}
  \item{useSmoothScatter}{boolean, set to \code{TRUE} to use 
    \code{\link{smoothScatter}} instead of \code{\link{plot}} to draw
    the plot.}
  \item{df}{optional argument, passed to \code{\link{swN2}} if provided, and
    if a plot using \eqn{N^2}{N^2} is requested.}
  \item{keepNA}{FALSE}
  \item{type}{type of plot to draw, using the same scheme as
    \code{\link{plot}}.}
  \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and also for
    \code{par(mar)}, computed from this.  The default is tighter than the R
    default, in order to use more space for the data and less for the axes.}
  \item{mar}{value to be used with \code{\link{par}("mar")}.}
  \item{add}{a flag that controls whether to add to an existing plot.  (It makes
    sense to use \code{add=TRUE} in the \code{panel} argument of a
    \code{\link{coplot}}, for example.)}
  \item{inset}{set to \code{TRUE} for use within \code{\link{plotInset}}.  The
    effect is to prevent the present function from adjusting margins, which is
    necessary because margin adjustment is the basis for the method used by
    \code{\link{plotInset}}.}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a moderate
    amount of debugging information, or to 2 to get more.}
  \item{\dots}{optional arguments passed to other functions.  A common example
    is to set \code{df}, for use in \link{swN2} calculations.} 
}

\details{The colours (\code{col.salinity}, etc.) are ony used if two profiles
  appear on a plot.}

\value{None.}

\seealso{\code{\link{read.ctd}} scans ctd information from a file,
  \code{\link{plot.ctd}} is a general plotting function for \code{ctd} objects,
  and \code{\link{plotTS}} plots a temperature-salinity diagrams. }

\examples{
library(oce)
data(ctd)
plotProfile(ctd, xtype="temperature")
}

\author{Dan Kelley}

\keyword{hplot}
