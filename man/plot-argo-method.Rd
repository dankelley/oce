% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/argo.R
\name{plot,argo-method}
\alias{plot,argo-method}
\alias{plot.argo}
\title{Plot an argo Object}
\usage{
\S4method{plot}{argo}(
  x,
  which = 1,
  level,
  coastline = c("best", "coastlineWorld", "coastlineWorldMedium", "coastlineWorldFine",
    "none"),
  cex = 1,
  pch = 1,
  type = "p",
  col = 1,
  fill = FALSE,
  projection = NULL,
  mgp = getOption("oceMgp"),
  mar = c(mgp[1] + 1.5, mgp[1] + 1.5, 1.5, 1.5),
  tformat,
  debug = getOption("oceDebug"),
  ...
)
}
\arguments{
\item{x}{an \linkS4class{argo} object.}

\item{which}{list of desired plot types, one of the following. Note
that \code{\link[=oce.pmatch]{oce.pmatch()}} is used to try to complete partial
character matches, and that an error will occur if the match is
not complete (e.g. \code{"salinity"} matches to both
\code{"salinity ts"} and \code{"salinity profile"}.).
\itemize{
\item \code{which=1}, \code{which="trajectory"} or \code{which="map"} gives a
plot of the argo trajectory, with the coastline, if one is provided.
\item \code{which=2} or \code{"salinity ts"} gives a time series of
salinity at the indicated level(s)
\item \code{which=3} or \code{"temperature ts"} gives a time series
of temperature at the indicated level(s)
\item \code{which=4} or \code{"TS"} gives a TS diagram at the
indicated level(s)
\item \code{which=5} or \code{"salinity profile"} gives a salinity profile
\item \code{which=6} or \code{"temperature profile"} gives a temperature profile
\item \code{which=7} or \code{"sigma0 profile"} gives a sigma0 profile
\item \code{which=8} or \code{"spice profile"} gives a spiciness
profile, referenced to the surface.  (This is the
same as using \code{which=9}.)
\item \code{which=9} or \code{"spiciness0 profile"} gives
a profile of spiciness referenced to a pressure
of 0 dbar, i.e. the surface.  (This is the
same as using \code{which=8}.)
\item \code{which=10} or \code{"spiciness1 profile"} gives
a profile of spiciness referenced to a pressure
of 1000 dbar.
\item \code{which=11} or \code{"spiciness2 profile"} gives
a profile of spiciness referenced to a pressure
of 2000 dbar.
}}

\item{level}{depth pseudo-level to plot, for \code{which=2} and higher.
May be an integer, in which case it refers to an index of depth (1
being the top) or it may be the string "all" which means to plot
all data.}

\item{coastline}{character string giving the coastline to be used
in an Argo-location map, or \code{"best"} to pick the one with highest
resolution, or \code{"none"} to avoid drawing the coastline.}

\item{cex}{size of plotting symbols to be used if \code{type="p"}.}

\item{pch}{type of plotting symbols to be used if \code{type="p"}.}

\item{type}{plot type, either \code{"l"} or \code{"p"}.}

\item{col}{optional list of colors for plotting.}

\item{fill}{either a logical, indicating whether to fill the land with
light-gray, or a color name.  Owing to problems with some projections, the
default is not to fill.}

\item{projection}{character value indicating the projection to be used
in trajectory maps. If this is \code{NULL}, no projection is used, although
the plot aspect ratio will be set to yield zero shape distortion at the
mean float latitude.  If \code{projection="automatic"}, then one
of two projections is used: stereopolar (i.e. \code{"+proj=stere +lon_0=X"}
where \code{X} is the mean longitude), or Mercator (i.e. \code{"+proj=merc"})
otherwise.  Otherwise, \code{projection} must be a character string specifying
a projection in the notation used by \code{\link[=oceProject]{oceProject()}} and \code{\link[=mapPlot]{mapPlot()}}.}

\item{mgp}{a 3-element numerical vector to use for \code{par(mgp)}, and also for
\code{par(mar)}, computed from this.  The default is tighter than the R
default, in order to use more space for the data and less for the axes.}

\item{mar}{value to be used with \code{par("mar")}.}

\item{tformat}{optional argument passed to \code{\link[=oce.plot.ts]{oce.plot.ts()}}, for plot
types that call that function.  (See \code{\link[=strptime]{strptime()}} for the format
used.)}

\item{debug}{debugging flag.}

\item{...}{optional arguments passed to plotting functions.}
}
\value{
None.
}
\description{
Plot a summary diagram for argo data.
}
\examples{
library(oce)
data(argo)
tc <- cut(argo[["time"]], "year")
# Example 1: plot map, which reveals float trajectory.
plot(argo, pch = as.integer(tc))
year <- substr(levels(tc), 1, 4)
data(topoWorld)
contour(topoWorld[["longitude"]], topoWorld[["latitude"]],
    topoWorld[["z"]],
    add = TRUE
)
legend("bottomleft", pch = seq_along(year), legend = year, bg = "white", cex = 3 / 4)

# Example 2: plot map, TS, T(z) and S(z). Note the use
# of handleFlags(), to skip over questionable data.
plot(handleFlags(argo), which = c(1, 4, 6, 5))

}
\seealso{
Other things related to argo data: 
\code{\link{D4902337_219.nc}},
\code{\link{[[,argo-method}},
\code{\link{[[<-,argo-method}},
\code{\link{argo}},
\code{\link{argo-class}},
\code{\link{argo2ctd}()},
\code{\link{argoGrid}()},
\code{\link{argoNames2oceNames}()},
\code{\link{as.argo}()},
\code{\link{handleFlags,argo-method}},
\code{\link{read.argo}()},
\code{\link{read.argo.copernicus}()},
\code{\link{subset,argo-method}},
\code{\link{summary,argo-method}}

Other functions that plot oce data: 
\code{\link{download.amsr}()},
\code{\link{plot,adp-method}},
\code{\link{plot,adv-method}},
\code{\link{plot,amsr-method}},
\code{\link{plot,bremen-method}},
\code{\link{plot,cm-method}},
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
}
\author{
Dan Kelley
}
\concept{functions that plot oce data}
\concept{things related to argo data}
