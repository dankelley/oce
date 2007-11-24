\name{plot.ctd}
\alias{plot.ctd}
\title{Plot seawater (CTD) data}
\description{
  Plot a summary diagram for CTD data.
}
\usage{
\method{plot}{ctd}(x,ref.lat=NaN, ref.lon=NaN, grid=TRUE, col.grid="lightgray", \dots)
}
\arguments{
  \item{x}{A \code{cdt} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{ref.lat}{Latitude of reference point for distance calculation}
  \item{ref.lon}{Longitude of reference point for distance calculation}
  \item{grid}{Set \code{TRUE} to get a grid on all plots.}
  \item{col.grid}{color of grid.}
  \item{\dots}{optional arguments passed to plotting functions. A common example is to set \code{df}, for 
use in \link{sw.N2} calculations.}
}
\details{
  Creates a summary plot for a CTD cast, with a temperature-salinity
  diagram as well as profiles of temperature, salinity, and density; a
  table of information about the cast is also plotted.
}
\value{
  None.
}
\seealso{
  \code{\link{summary.ctd}} summarizes the information, while
  \code{\link{read.ctd}} scans it from a file.
}
\examples{
library(oce)
data(ctd) 
plot(ctd)
}
\references{
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}
