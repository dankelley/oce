\name{plot.ctd.scan}
\alias{plot.ctd.scan}
\title{Plot seawater (CTD) data}
\description{
  Plot CTD data as time-series against scan number, to aide in trimming.
}
\usage{
plot.ctd.scan(x, name = "scan", 
 S.col = "darkgreen", T.col= "darkred", p.col = "blue", \dots)
}
\arguments{
  \item{x}{A \code{cdt} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{name}{name of variable for x axis}
  \item{S.col}{colour for salinity}
  \item{T.col}{colour for temperature}
  \item{p.col}{colour for pressure}
  \item{\dots}{optional arguments passed to plotting functions.}
}
\details{
  Plots CTD data as time-series against the scan number, as an 
  aide to trimming to downcasts, \emph{etc}.
}
\value{
  None.
}
\seealso{
  \code{\link{summary.ctd}} summarizes a CTD object
  \code{\link{plot.ctd}} plot summary diagram of CTD object.
  \code{\link{read.ctd}} scans CTD object from a file.
}
\examples{
library(oce)
data(ctd) 
plot.ctd.scan(ctd)
}
\author{Dan Kelley}
\keyword{misc}
