\name{plot.ctd}

\alias{plot.ctd}

\title{Plot seawater CTD data}

\description{Plot a summary diagram for CTD data.}

\usage{\method{plot}{ctd}(x,ref.lat=NaN, ref.lon=NaN, grid=TRUE, col.grid="lightgray", 
  textpanel=TRUE, \dots)
}
\arguments{
  \item{x}{A \code{cdt} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{ref.lat}{Latitude of reference point for distance calculation}
  \item{ref.lon}{Longitude of reference point for distance calculation}
  \item{grid}{Set \code{TRUE} to get a grid on all plots.}
  \item{col.grid}{The color of the grid.}
  \item{textpanel}{Set TRUE for bottom-right panel to give a textual overview; set FALSE if you'd like to plot this panel yourself (as the 4th member of a 2-by-2 grid).}
  \item{\dots}{optional arguments passed to plotting functions. A common example is to set \code{df}, for use in \link{sw.N2} calculations.}
}

\details{Creates a four-panel summary plot of data measured in a CTD
  cast. One panel contains profiles of temperature and salinity, another
  contains profiles of density and the square of buoyancy frequency, and
  a third contains a TS diagram.  The fourth panel provides information
  about the cast.  }

\seealso{The profiles are plotted with \code{\link{plot.profile}}, and
  the TS diagram is plotted with \code{\link{plot.TS}}.  CTD data can be
  created with \code{\link{as.ctd}} or read with \code{\link{read.ctd}}.
  Summaries of CTD data are provided by \code{\link{summary.ctd}}.  }

\examples{
library(oce)
data(ctd) 
plot(ctd)
}
\author{Dan Kelley}
\keyword{misc}
