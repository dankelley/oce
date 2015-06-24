\name{plotTS}

\alias{plotTS}

\title{Plot temperature-salinity diagram}

\description{Plot temperature-salinity diagram for seawater (CTD) data.}

\usage{plotTS(x,
    inSitu=FALSE,
    type="p",
    referencePressure=0,
    nlevels=6, levels,
    grid=TRUE,
    col.grid="lightgray",
    lty.grid="dotted",
    rho1000=FALSE,
    eos=getOption("oceEOS", default='gsw'),
    cex=par("cex"), col=par("col"), pch=par("pch"),
    bg, pt.bg="transparent",
    col.rho="darkgray",
    cex.rho=3/4*par("cex"),
    rotate=TRUE,
    useSmoothScatter=FALSE,
    xlab,
    ylab,
    Slim,
    Tlim,
    mgp=getOption("oceMgp"),
    mar=c(mgp[1]+1.5,mgp[1]+1.5,mgp[1],mgp[1]),
    lwd=par('lwd'), lty=par('lty'),
    lwd.rho=par("lwd"), lty.rho=par("lty"),
    add=FALSE, inset=FALSE,
    debug=getOption("oceDebug"),
    \dots)
}

\arguments{
    \item{x}{An object containing salinity and temperature data, typically a
        \code{ctd} object or \code{section} object.}
    \item{inSitu}{A boolean indicating whether to use in-situ temperature or
        (the default) potential temperature, calculated with reference pressure
        given by \code{referencePressure}.  This is ignored if \code{eos="gsw"},
        because those cases the y axis is necessarily the
        conservative formulation of temperature.}
    \item{type}{representation of data, \code{"p"} for points, \code{"l"} for
        connecting lines, or \code{"n"} for no indication.}
    \item{referencePressure}{reference pressure, to be used in calculating
        potential temperature, if \code{inSitu} is \code{FALSE}.}
    \item{nlevels}{Number of automatically-selected isopycnal levels (ignored
        if \code{levels} is supplied).}
    \item{levels}{Optional vector of desired isopycnal levels.}
    \item{grid}{a flag that can be set to \code{TRUE} to get a grid.}
    \item{col.grid}{colour for grid.}
    \item{lty.grid}{line type for grid.}
    \item{rho1000}{if TRUE, label isopycnals as e.g. 1024; if FALSE, label as e.g. 24}
    \item{eos}{equation of state to be used, either \code{"unesco"} or
        \code{"gsw"}.}
    \item{cex}{character-expansion factor for symbols, as in \code{\link{par}("cex")}.}
    \item{pch}{symbol type, as in \code{\link{par}("pch")}.}
    \item{bg}{optional colour to be painted under plotting area, before plotting.
        (This is useful for cases in which \code{inset=TRUE}.)}
    \item{pt.bg}{inside color for symbols with \code{pch} in 21:25}
    \item{col}{colour for symbols.}
    \item{col.rho}{colour for isopycnal lines.}
    \item{cex.rho}{size of isopycnal labels.}
    \item{rotate}{if TRUE, labels in right-hand margin are written vertically}
    \item{useSmoothScatter}{if TRUE, use \code{\link{smoothScatter}} to plot the points.}
    \item{xlab}{optional label for the x axis, with default "Salinity [PSU]".}
    \item{ylab}{optional label for the y axis, with default "Temperature [C]".}
    \item{Slim}{optional limits for salinity axis, otherwise inferred from data.}
    \item{Tlim}{optional limits for temperature axis, otherwise inferred from data.}
    \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
        also for \code{par(mar)}, computed from this.  The default is
        tighter than the R default, in order to use more space for the data
        and less for the axes.}
    \item{mar}{value to be used with \code{\link{par}("mar")}.  If set to
        \code{NULL}, then \code{par("mar")} is used.  A good choice for a TS
        diagram with a palette to the right is \code{mar=par("mar")+c(0, 0, 0,
            1))}.} 
    \item{lwd}{line width of lines or symbols.}
    \item{lty}{line type of lines or symbols.}
    \item{lwd.rho}{line width for density curves.}
    \item{lty.rho}{line type for density curves.}
    \item{add}{a flag that controls whether to add to an existing plot.  (It
        makes sense to use \code{add=TRUE} in the \code{panel} argument of a
        \code{\link{coplot}}, for example.)}
    \item{inset}{set to \code{TRUE} for use within \code{\link{plotInset}}.  The
        effect is to prevent the present function from adjusting margins, which
        is necessary because margin adjustment is the basis for the method
        used by \code{\link{plotInset}}.}
    \item{debug}{a flag that turns on debugging.  Set to 1 to get a
        moderate amount of debugging information, or to 2 to get more.}
    \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Creates a temperature-salinity plot for a CTD cast, with labeled isopycnals.}

\value{A list is silently returned, containing \code{xat} and \code{yat},
    values that can be used by \code{\link{oce.grid}} to add a grid to the
    plot.}

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
