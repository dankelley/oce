\name{plotTS}

\alias{plotTS}

\title{Plot temperature-salinity diagram for seawater (CTD) data}

\description{Plot temperature-salinity diagram for seawater (CTD) data.}

\usage{plotTS(x,
     inSitu=FALSE,
     referencePressure=0,
     rhoLevels=6,
     grid=TRUE,
     col.grid="lightgray",
     lty.grid="dotted",
     rho1000=FALSE,
     cex=par("cex"), col=par("col"), pch=par("pch"),
     col.rho="darkgray",
     cex.rho=3/4*par("cex"),
     rotateRhoLabels=FALSE,
     connectPoints=FALSE,
     useSmoothScatter=FALSE,
     xlab,
     ylab,
     Slim,
     Tlim,
     mgp=getOption("oceMgp"),
     mar=c(mgp[1]+1,mgp[1]+1,mgp[1],mgp[1]),
     lwd.rho=par("lwd"), lty.rho=par("lty"),
     add=FALSE,
     debug=getOption("oceDebug"),
     \dots)
}

\arguments{
  \item{x}{An object containing salinity and temperature data, typically a
      \code{cdt} object or \code{section} object.}
  \item{inSitu}{A boolean indicating whether to use in-situ temperature or (the
      default) potential temperature, calculated with reference pressure given
      by \code{referencePressure}.}
  \item{referencePressure}{reference pressure, to be used in calculating
      potential temperature, if \code{inSitu} is \code{FALSE}.}
  \item{rhoLevels}{Either a list of density levels for which to draw isopycnal
      lines, or a suggestion for the number of levels.  In the latter case,
      \code{pretty()} is used to select levels.}
  \item{grid}{a flag that can be set to \code{TRUE} to get a grid.}
  \item{col.grid}{colour for grid.}
  \item{lty.grid}{line type for grid.}
  \item{rho1000}{if TRUE, label isopycnals as e.g. 1024; if FALSE, label as e.g. 24}
  \item{cex}{character-expansion factor for symbols, as in \code{\link{par}("cex")}.}
  \item{pch}{symbol type, as in \code{\link{par}("pch")}.}
  \item{col}{colour for symbols.}
  \item{col.rho}{colour for isopycnal lines.}
  \item{cex.rho}{size of isopycnal labels.}
  \item{rotateRhoLabels}{if TRUE, labels in right-hand margin are written vertically}
  \item{connectPoints}{if TRUE, the points are connected with line segments, in sequence}
  \item{useSmoothScatter}{if TRUE, use \code{\link{smoothScatter}} to plot the points.}
  \item{xlab}{optional label for the x axis, with default "Salinity [PSU]".}
  \item{ylab}{optional label for the y axis, with default "Temperature [C]".}
  \item{Slim}{optional limits for salinity axis, otherwise inferred from data.}
  \item{Tlim}{optional limits for temperature axis, otherwise inferred from data.}
  \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
    also for \code{par(mar)}, computed from this.  The default is
    tighter than the R default, in order to use more space for the data
    and less for the axes.}
  \item{mar}{value to be used with \code{\link{par}("mar")}.}
  \item{lwd.rho}{line width for density lines.  If \code{connectPoints=TRUE},
      then the line connecting points may be controlled by including a
      \code{lwd} value in the optional arguments.}
  \item{lty.rho}{line type for density lines.  (See the note about
      \code{connectPoints} for \code{lwd.rho}.)},
  \item{add}{a flag that controls whether to add to an existing plot.  (It
      makes sense to use \code{add=TRUE} in the \code{panel} argument of a
      \code{\link{coplot}}, for example.)}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
      moderate amount of debugging information, or to 2 to get more.}
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
data(ctd)
plotTS(ctd)
}

\author{Dan Kelley}

\keyword{hplot}
