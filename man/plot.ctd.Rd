\name{plot.ctd}

\alias{plot.ctd}
\alias{plot,ctd,missing-method}
\alias{plot,ctd-method}

\title{Plot seawater CTD data}

\description{Plot CTD data, by default in a four-panel display showing (a)
    profiles of salinity and temperature, (b) profiles of density and the
    square of buoyancy frequency, (c) a TS diagram and (d) a coastline diagram
    indicating the station location.}

\usage{\S4method{plot}{ctd}(x, which=c(1, 2, 3, 5), 
     eos=getOption("eos", default='unesco'),
     ref.lat=NaN, ref.lon=NaN, 
     grid=TRUE, col.grid="lightgray", lty.grid="dotted",
     coastline="best",
     Slim, Tlim, plim, densitylim, N2lim, Rrholim,
     dpdtlim, timelim,
     lonlim, latlim, span,
     latlon.pch=20, latlon.cex=1.5, latlon.col="red",
     cex=1, cex.axis=par('cex.axis'),
     pch=1,
     useSmoothScatter=FALSE,
     df,
     keepNA=FALSE,
     type='l',
     adorn=NULL,
     mgp=getOption("oceMgp"),
     mar=c(mgp[1]+1.5,mgp[1]+1.5,mgp[1]+1.5,mgp[1]+1),
     inset=FALSE,
     add=FALSE,
     debug=getOption("oceDebug"),
     \dots)
}
\arguments{
  \item{x}{A \code{ctd} object, e.g. as read by \code{\link{read.ctd}}, or a
      list containing items named \code{salinity} and \code{temperature}.}
  \item{which}{list of desired plot types.  
      \itemize{
      \item \code{which=1} or \code{which="salinity+temperature"} gives
      a combined profile of temperature and salinity
      \item \code{which=2} or \code{which="density+N2"} gives a combined
      profile of \eqn{\sigma_\theta}{sigma-theta} and \eqn{N^2}{N^2}
      \item \code{which=3} or \code{which="TS"} gives a TS plot
      \item \code{which=4} or \code{which="text"} gives a textual
      summary of some aspects of the data
      \item \code{which=5} or \code{which="map"} gives a map, with a dot
      for the station location.  If a coastline is given, that is drawn as
      well.  Notes near the top boundary of the map give the station number,
      the sampling date, and the name of the chief scientist, if these are
      known.
      \item \code{which=5.1} as for \code{which=5}, except that the file name
      is drawn above the map
      \item \code{which=6} or \code{which="density+dpdt"} gives a
      profile of density and \eqn{dP/dt}{dP/dt}, which is useful for
      evaluating whether the instrument is dropping properly through the
      water column
      \item \code{which=7} or \code{which="density+time"} gives a
      profile of density and time
      \item \code{which=8} or \code{which="index"} gives a profile of
      index number (especially useful for \code{\link{ctdTrim}})
      \item \code{which=9} or \code{which="salinity"} gives a salinity profile
      \item \code{which=10} or \code{which="temperature"} gives a temperature profile
      \item \code{which=11} or \code{which="density"} gives a density profile
      \item \code{which=12} or \code{which="N2"} gives an \eqn{N^2}{N^2} profile
      \item \code{which=13} or \code{which="spice"} gives a spiciness profile
      \item \code{which=14} or \code{which="tritium"} gives a tritium profile
    }
  }
  \item{eos}{either \code{"unesco"} or \code{"teos"}.  If the latter, then the
      computer must have the TEOS library installed; see \code{\link{teos}}.
      The effect of using \code{"teos"} is to us ``absolute salinity'' on the x
      axis and ``conservative temperature'' on the y axis.  Typically, the
      temperature values will be very similar to those with \code{unesco}, but
      the salinities will be increased by 0.1 to 0.2 units.  The isopycnals
      will still run through the same points as for the \code{unesco} style.
      See \code{\link{teos}} for more information.}
  \item{ref.lat}{Latitude of reference point for distance calculation}
  \item{ref.lon}{Longitude of reference point for distance calculation}
  \item{grid}{Set \code{TRUE} to get a grid on all plots.}
  \item{col.grid}{colour for the grid.}
  \item{lty.grid}{line type for grid.}
  \item{coastline}{a specification of the coastline to be used for
      \code{which="map"}.  This may be a coastline object, whether built-in or
      supplied by the user, or a character string.  If the later, it may be the
      name of a built-in coastline (\code{"coastlineWorld"},
      \code{"coastlineWorldFine"}, or
      \code{"coastlineWorldCoarse"}), or \code{"best"}, to choose 
      a suitable coastline for the locale, or \code{"none"} to prevent
      the drawing of a coastline.  There is a speed penalty for providing
      \code{coastline} as a character string, because it forces
      \code{\link{plot.coastline}} to load it on every call.  So, if
      \code{plot.coastline} is to be called several times for a given
      coastline, it makes sense to load it in before the first call, and to
      supply the object as an argument, as opposed to the name of the object.}
  \item{Slim}{optional limits of salinity axes}
  \item{Tlim}{optional limits of temperature axes}
  \item{plim}{optional limits of pressure axes}
  \item{densitylim}{optional limits of density axis}
  \item{N2lim}{optional limits of \eqn{N^2}{N^2} axis}
  \item{Rrholim}{optional limits of \eqn{R_rho}{R_rho} axis}
  \item{dpdtlim}{optional limits of dP/dt axis}
  \item{timelim}{optional limits of delta-time axis}
  \item{lonlim}{optional limits of longitude axis of map (ignored if no
    map plotted)} 
  \item{latlim}{optional limits of latitude axis of map (ignored if no
    map plotted)} 
  \item{span}{optional span of map, in km (ignored unless a map is being drawn)}
  \item{latlon.pch}{pch for sample location (ignored if no map plotted)}
  \item{latlon.cex}{cex for sample location (ignored if no map plotted)}
  \item{latlon.col}{col for sample location (ignored if no map plotted)}
  \item{cex}{size to be used for plot symbols (see \code{\link{par}})}
  \item{cex.axis}{size factor for axis labels (see \code{\link{par}})}
  \item{pch}{code for plotting symbol (see \code{\link{par}}).}
  \item{useSmoothScatter}{boolean, set to \code{TRUE} to use 
    \code{\link{smoothScatter}} instead of \code{\link{plot}} to draw
    the plot.}

  \item{df}{optional argument that is ignored except for plotting buoyancy
      frequency; in that case, it is passed to \code{\link{swN2}} as the
      argument named \code{df}.}

  \item{keepNA}{flag indicating whether to keep \code{NA} values in linegraphs,
      which will yield breaks in the lines.}
  \item{type}{type of plot to draw, using the same scheme as \code{\link{plot}}.}
  \item{adorn}{list of expressions to be executed for the panels in
    turn, e.g. to adorn the plots.  If the number matches the number of
    panels, then the strings are applied to the appropriate panels, 
    as they are drawn from top-left to bottom-right.   If only a single
    expression is provided, it is used for all panels.  (See
    \dQuote{Examples}.)}
  \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
    also for \code{par(mar)}, computed from this.  The default is
    tighter than the R default, in order to use more space for the data
    and less for the axes.}
  \item{mar}{value to be used with \code{\link{par}("mar")}.}
  \item{inset}{set to \code{TRUE} for use within \code{\link{plotInset}}.  The
      effect is to prevent the present function from adjusting margins, which
      is necessary because margin adjustment is the basis for the method
      used by \code{\link{plotInset}}.}
  \item{add}{add to an existing plot.  This only works if
      \code{length(which)=1}, and it will yield odd results if the value of
      \code{which} does not match that in the previous plots.}
  \item{debug}{set to a positive value to get debugging information during processing.}
  \item{\dots}{optional arguments passed to plotting functions. A common
    example is to set \code{df}, for use in \link{swN2} calculations.} 
}

\details{Creates a multi-panel summary plot of data measured in a CTD
  cast. The panels are controlled by the \code{which} argument.
  Normally, 4 panels are specified with the \code{which}, but it can
  also be useful to specify less than 4 panels, and then to draw other
  panels after this call.

  If only 2 panels are requested, they will be drawn side by side.

  If more than one panel is drawn, then on exit from \code{plot.ctd},
  the value of \code{par} will be reset to the value it had before the
  function call.  However, if only one panel is drawn, the adjustments
  to \code{par} made within \code{plot.ctd} are left in place, so that
  further additions may be made to the plot.
}

\seealso{The documentation for \code{\link{ctd-class}} explains the structure
    of CTD objects, and also outlines the other functions dealing with them.}

\examples{
library(oce)
data(ctd) 
plot(ctd)
}
\author{Dan Kelley}
\keyword{hplot}
