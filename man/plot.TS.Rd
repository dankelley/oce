\name{plot.TS}
\alias{plot.TS}
\title{Plot temperature-salinity diagram for seawater (CTD) data}
\description{
  Plot temperature-salinity diagram for seawater (CTD) data.
}
\usage{
plot.TS(x,
	rho.levels = 6,
	grid = FALSE,
	col.grid = "lightgray",
	rho1000 = FALSE,
	col = par("col"),
	col.rho = "darkgray",
	cex.rho = 0.9*par("cex"),
	cex = par("cex"),
	pch = 20,
	rotate.rho.labels = FALSE,
	connect.points = FALSE,
	\dots)
}
\arguments{
  \item{x}{A \code{cdt} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{rho.levels}{Either a list of density levels for which to draw 
                    isopycnal lines, or a suggestion for the number of levels.
 					In the latter case, \code{pretty()} is used to select levels.}
  \item{grid}{a flag that can be set to \code{TRUE} to get a grid.}
  \item{col.grid}{color of grid.}
  \item{rho1000}{if TRUE, label isopycnals as e.g. 1024; if FALSE, label as e.g. 24}
  \item{col}{colour for symbols.}
  \item{col.rho}{colour for isopycnal lines.}
  \item{cex.rho}{size of isopycnal labels.}
  \item{cex}{size of symbols on graph.}
  \item{pch}{code for symbols on graph.}
  \item{rotate.rho.labels}{if TRUE, labels in right-hand margin are written vertically}
  \item{connect.points}{if TRUE, the points are connected with line segments, in sequence}
  \item{\dots}{optional arguments passed to plotting functions.}
}
\details{
  Creates a temperature-salinity plot for a CTD cast, with labeled isopycnals.
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
# Plot TS diagram for data along 36N Atlantic section,
# colour-coded for longitude (which ranges -8 to -74)
data(a03)
for (i in 1:124) {
  profile <- a03$data$station[[i]]
  x <- (-8 - profile$metadata$longitude) / 66
  col <- hsv(0.666*x)
  if (i == 1) {
       plot.TS(profile, xlim=c(34,37), ylim=c(2,25), col=col)
  } else {
       points(profile$data$salinity, profile$data$temperature,pch=20,col=col)
  }
}
title("North Atlantic along 37N from America (blue) to Europe (red)")
}
\author{Dan Kelley}
\keyword{misc}
