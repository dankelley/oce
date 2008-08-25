\name{plot.TS}
\alias{plot.TS}
\title{Plot temperature-salinity diagram for seawater (CTD) data}
\description{Plot temperature-salinity diagram for seawater (CTD) data.}
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
	pch = 21,
	rotate.rho.labels = FALSE,
	connect.points = FALSE,
	xlab,
	ylab,
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
  \item{xlab}{optional label for the x axis, with default "Salinity [PSU]".}
  \item{ylab}{optional label for the y axis, with default "Temperature [C]".}
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Creates a temperature-salinity plot for a CTD cast, with labeled isopycnals.}

\value{None.}

\seealso{
  \code{\link{summary.ctd}} summarizes the information, while
  \code{\link{read.ctd}} scans it from a file.
}
\examples{
library(oce)
# Plot TS diagram for a mid-Atlantic station at 36N
# See demo(TS) for a section-spanning TS diagram
data(a03)
profile <- a03$data$station[[60]] # 124 stations in total
plot.TS(profile)
title("Mid-Atlantic at 36N")
}
\author{Dan Kelley}
\keyword{misc}
