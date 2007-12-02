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
	cex.rho = 0.8*par("cex"),
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
\dontrun{
library(oce)
profile <- read.ctd("/usr/local/lib/R/library/oce/demo/ctdprofile.cnv")
attach(profile)
plot.TS(profile)
# North Atlantic thermocline water, mixing to Antarctic Intermediate Water
# (after Defant's analysis)
N <- 10
Srange <- c(34.15, 35.94)
Trange <- c(3.3,18)
SS <- Srange[1] + (Srange[2] - Srange[1]) *(1:N)/(N-1) + 0.01*rnorm(N)
TT <- Trange[1] + (Trange[2] - Trange[1]) *(1:N)/(N-1) + 0.01*rnorm(N)
plot.TS(as.CTD(S=SS, T=TT, p=0))
}
}
\references{}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}
