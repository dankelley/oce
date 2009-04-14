\name{plot.ctd}

\alias{plot.ctd}

\title{Plot seawater CTD data}

\description{Plot a summary diagram for CTD data.}

\usage{\method{plot}{ctd}(x,ref.lat=NaN, ref.lon=NaN, 
  grid=TRUE, col.grid="lightgray", lty.grid="dotted",
  which=1:4, coastline,
  Slim, Tlim, plim, densitylim, dpdtlim, timelim,
  lonlim, latlim,
  latlon.pch=20, latlon.cex=1.5, latlon.col="red",
  adorn=NULL,
  \dots)
}
\arguments{
  \item{x}{A \code{cdt} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{ref.lat}{Latitude of reference point for distance calculation}
  \item{ref.lon}{Longitude of reference point for distance calculation}
  \item{grid}{Set \code{TRUE} to get a grid on all plots.}
  \item{col.grid}{colour for the grid.}
  \item{lty.grid}{line type for grid.}
  \item{which}{list of desired plot types.  There are four panels in
    total, and the desired plots are place in these, in reading order.
    Which=1 gives a combined profile of temperature and salinity.
    \code{which}=2 gives a combined profile of sigma-theta and
    \eqn{N^2}{N^2}. \code{which}=3 gives a TS plot.
    \code{which}=4 gives a textual summary of some aspects of the data.
    \code{which}=5 gives a map, with a dot for the station
    location (note that this requires that a \code{coastline} be
    given).  \code{which}=6 gives a profile of density and dP/dt (useful
    for evaluating whether the instrument is dropping properly through
  the water column).  \code{which}=7 gives a profile of density and
  time. \code{which}=8 gives a profile of index number (especially
  useful for \code{\link{ctd.trim}}).  \code{which}=9 gives a plot of density.}
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
  \item{adorn}{list of character strings containing commands to be
    executed for the panels.  If the number matches the number of
    panels, then the strings are applied to the appropriate panels,
    as they are drawn from top-left to bottom-right.   If only a single
    string is provided, it is used for all panels.  (See
    \dQuote{Examples}.)}
  \item{\dots}{optional arguments passed to plotting functions. A common
    example is to set \code{df}, for use in \link{sw.N2} calculations.} 
}

\details{Creates a multi-panel summary plot of data measured in a CTD
  cast. The panels are controlled by the \code{which} argument.
  Normally, 4 panels are specified with the \code{which}, but it can
  also be useful to specify less than 4 panels, and then to draw other
  panels after this call.

  If only 2 panels are requested, they will be drawn side by side.

  If only one panel is drawn, \code{par} will not be altered, which
  means that you may follow this call with other plotting commands, and
  the results will appear on the plot (which is not the case for the
  multi-panel forms).  This is very useful in trimming the data,
  e.g. you might do \code{plot(ctd, which=8)} and notice that downcast
  is between index 10 and 300; then try 
  \code{plot(ctd.trim(ctd, "index", 10, 300), which=8)} to see if your
  guess is right, and thus gradually you can find good settings for
  trimming; see also \code{\link{plot.ctd.scan}}.}

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
plot(ctd, which=c(1,2,3,5),coastline=coastline.sle,lonlim=lonlim,latlim=latlim,
          adorn=c("","","","text(-69.72,47.85,'IAL',pos=4)"))

# Trimming
data(ctd.raw)
plot(ctd.raw, which=8)
abline(v=100)
abline(v=120)
abline(v=130)
abline(v=350)
# The range 130 to 350 seems good, but you can narrow in
# by repeating the command below, with differing ranges.
plot(ctd.trim(ctd.raw, "index", 130:350), which=8)
# See results
ctd.trimmed <- ctd.trim(ctd.raw, "index", 130:350)
plot(ctd.trimmed)
}
\author{Dan Kelley}
\keyword{misc}
