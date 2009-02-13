\name{plot.ctd}

\alias{plot.ctd}

\title{Plot seawater CTD data}

\description{Plot a summary diagram for CTD data.}

\usage{\method{plot}{ctd}(x,ref.lat=NaN, ref.lon=NaN, 
  grid=TRUE, col.grid="lightgray",
  which=1:4, coastline,
  Slim, Tlim, plim, densitylim, dpdtlim,
  lonlim, latlim,
  latlon.pch=20, latlon.cex=1.5, latlon.col="red",
  \dots)
}
\arguments{
  \item{x}{A \code{cdt} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{ref.lat}{Latitude of reference point for distance calculation}
  \item{ref.lon}{Longitude of reference point for distance calculation}
  \item{grid}{Set \code{TRUE} to get a grid on all plots.}
  \item{col.grid}{The color of the grid.}
  \item{which}{list of desired plot types.  There are four panels in
    total, and the desired plots are place in these, in reading order.
    Which=1 for a combined profile of temperature and salinity.
    \code{which}=2 yields a combined profile of sigma-theta and
    \eqn{N^2}{N^2}, while \code{which}=3 yields a TS plot,
    \code{which}=4 yields a textual summary of some aspects of the data,
    and \code{which}=5 yields a map, with a dot for the station
    location.  (The map form requires that a \code{coastline} be
    given.)}
  \item{coastline}{a \code{coastline} object, e.g. as read by
    \code{\link{read.coastline}} (ignored if no map plotted).}
  \item{Slim}{optional limits of salinity axes}
  \item{Tlim}{optional limits of temperature axes}
  \item{plim}{optional limits of pressure axes}
  \item{densitylim}{optional limits of density axis}
  \item{dpdtlim}{optional limits of dP/dt axis}
  \item{lonlim}{optional limits of longitude axis of map (ignored if no
    map plotted)} 
  \item{latlim}{optional limits of latitude axis of map (ignored if no
    map plotted)} 
  \item{latlon.pch}{pch for sample location (ignored if no map plotted)}
  \item{latlon.cex}{cex for sample location (ignored if no map plotted)}
  \item{latlon.col}{col for sample location (ignored if no map plotted)}
  \item{\dots}{optional arguments passed to plotting functions. A common
    example is to set \code{df}, for use in \link{sw.N2} calculations.} 
}

\details{Creates a multi-panel summary plot of data measured in a CTD
  cast. The panels are controlled by the \code{which} argument.
  Normally, 4 panels are specified with the \code{which}, but it can
  also be useful to specify less than 4 panels, and then to draw other
  panels after this call.}

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
plot(ctd, which=c(1,2,3,5),coastline=coastline.sle,lonlim=lonlim,latlim=latlim)
}
\author{Dan Kelley}
\keyword{misc}
