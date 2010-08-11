\name{plot.ctd}

\alias{plot.ctd}

\title{Plot seawater CTD data}

\description{Plot a summary diagram for CTD data.}

\usage{\method{plot}{ctd}(x,
     which=1:4, 
     ref.lat=NaN,
     ref.lon=NaN, 
     grid=TRUE,
     col.grid="lightgray",
     lty.grid="dotted",
     coastline,
     Slim, 
     Tlim, 
     plim,
     densitylim, 
     dpdtlim,
     timelim,
     lonlim,
     latlim,
     latlon.pch=20,
     latlon.cex=1.5,
     latlon.col="red",
     adorn=NULL,
     mgp=getOption("oce.mgp"),
     mar=c(mgp[1]+1,mgp[1]+1,mgp[1]+1.5,mgp[1]),
     debug=getOption("oce.debug"),
     \dots)
}
\arguments{
  \item{x}{A \code{cdt} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{which}{list of desired plot types.  
    \itemize{
      \item \code{which=1} or \code{which="temperature+salinity"} gives
      a combined profile of temperature and salinity
      \item \code{which=2} or \code{which="density+N2"} gives a combined
      profile of \eqn{\sigma_\theta}{sigma-theta} and \eqn{N^2}{N^2}
      \item \code{which=3} or \code{which="TS"} gives a TS plot
      \item \code{which=4} or \code{which="text"} gives a textual
      summary of some aspects of the data
      \item \code{which=5} or \code{which="map"} gives a map, with a dot
      for the station location (note that this requires that a
      \code{coastline} be given)
      \item \code{which=6} or \code{which="density+dpdt"} gives a
      profile of density and \eqn{dP/dt}{dP/dt}, which is useful for
      evaluating whether the instrument is dropping properly through the
      water column
      \item \code{which=7} or \code{which="density+time"} gives a
      profile of density and time
      \item \code{which=8} or \code{which="index"} gives a profile of
      index number (especially useful for \code{\link{ctd.trim}})
      \item \code{which=9} or \code{which="salinity"} gives a salinity profile
      \item \code{which=10} or \code{which="temperature"} gives a temperature profile
      \item \code{which=11} or \code{which="density"} gives a density profile
      \item \code{which=12} or \code{which="N2"} gives an \eqn{N^2}{N^2} profile
    }
  }
  \item{ref.lat}{Latitude of reference point for distance calculation}
  \item{ref.lon}{Longitude of reference point for distance calculation}
  \item{grid}{Set \code{TRUE} to get a grid on all plots.}
  \item{col.grid}{colour for the grid.}
  \item{lty.grid}{line type for grid.}
  \item{coastline}{a \code{coastline} object, e.g. as read by
    \code{\link{read.coastline}} (ignored if no map plotted).}
  \item{Slim}{optional limits of salinity axes}
  \item{Tlim}{optional limits of temperature axes}
  \item{plim}{optional limits of pressure axes}
  \item{densitylim}{optional limits of density axis}
  \item{dpdtlim}{optional limits of dP/dt axis}
  \item{timelim}{optional limits of delta-time axis}
  \item{lonlim}{optional limits of longitude axis of map (ignored if no
    map plotted)} 
  \item{latlim}{optional limits of latitude axis of map (ignored if no
    map plotted)} 
  \item{latlon.pch}{pch for sample location (ignored if no map plotted)}
  \item{latlon.cex}{cex for sample location (ignored if no map plotted)}
  \item{latlon.col}{col for sample location (ignored if no map plotted)}
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
  \item{debug}{set to \code{TRUE} to get debugging information during processing.}
  \item{\dots}{optional arguments passed to plotting functions. A common
    example is to set \code{df}, for use in \link{sw.N2} calculations.} 
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

\seealso{The profiles are plotted with \code{\link{plot.profile}}, and
  the TS diagram is plotted with \code{\link{plot.TS}}.  CTD data can be
  created with \code{\link{as.ctd}} or read with \code{\link{read.ctd}}.
  Summaries of CTD data are provided by \code{\link{summary.ctd}}.}

\examples{
library(oce)
data(ctd) 
plot(ctd)

data(coastline.sle)
lonlim <- -(69 + c(52, 41) / 60)
latlim <- 47 + c(48, 58) / 60
# IAL = Ile aux Lievres
plot(ctd, which=c(1,2,3,5),
     coastline=coastline.sle, lonlim=lonlim, latlim=latlim,
     adorn=expression({},{},{},{text(-69.72,47.85,'IAL',pos=4)}))

# Trimming
data(ctd.raw)
plot(ctd.raw, which=8)
abline(v=100)
abline(v=120)
abline(v=130)
abline(v=350)
# The range 130 to 350 seems good, but you can narrow in
# by repeating the command below, with differing ranges.
plot(ctd.trim(ctd.raw, "range", list(item="index", from=130, to=350)), which=8)
# See results
ctd.trimmed <- ctd.trim(ctd.raw, "range", list(item="index", from=130, to=350))
plot(ctd.trimmed)
}
\author{Dan Kelley}
\keyword{misc}
