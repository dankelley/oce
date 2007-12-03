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
  \item{col.grid}{The color of the grid.}
  \item{\dots}{optional arguments passed to plotting functions. A common example is to set \code{df}, for use in \link{sw.N2} calculations.}
}
\details{
  Creates a four-panel summary plot of data measured in a CTD cast. 
One panel shows profiles of temperature and salinity, a second
profiles of density and the square of buoyancy frequency, and a third a TS diagram.  The fourth panels is a panel of information about the cast.
}
\value{
  None.
}
\seealso{
  The profiles are plotted with \code{\link{plot.profile}}, and the TS diagram is plotted
with \code{\link{plot.TS}}.  CTD data can be created with \code{\link{as.CTD}} or read with
  \code{\link{read.ctd}}.  Summaries of CTD data are provided by
  \code{\link{summary.ctd}}.
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
