\name{oce.plot.ts}
\alias{oce.plot.ts}
\encoding{UTF-8}
\title{Plot a time-series, obeying the timezone}

\description{Plot a time-series, obeying the timezone and possibly drawing the
    range in the top-left margin}

\usage{
oce.plot.ts(x, y, type="l", xlim, ylim, xlab, ylab,
            drawTimeRange=getOption("oceDrawTimeRange"), adorn=NULL, fill=FALSE,
            xaxs=par("xaxs"), yaxs=par("yaxs"),
            cex=par("cex"), cex.axis=par("cex.axis"), cex.main=par("cex.main"),
            mgp=getOption("oceMgp"),
            mar=c(mgp[1]+if(nchar(xlab)>0) 1.5 else 1, mgp[1]+1.5, mgp[2]+1, mgp[2]+3/4),
            main="",
            despike=FALSE,
            axes=TRUE, tformat,
            marginsAsImage=FALSE,
            grid=FALSE, grid.col="darkgray", grid.lty="dotted", grid.lwd=1,
            debug=getOption("oceDebug"),
            \dots)}

\arguments{
\item{x}{the times of observations.}
\item{y}{the observations.}
\item{type}{plot type, \code{"l"} for lines, \code{"p"} for points.}
\item{xlim}{optional limit for x axis.  This has an additional effect, beyond
    that for conventional R functions: it effectively windows the data, so that
    autoscaling will yield limits for y that make sense within the window.}
\item{ylim}{optional limit for y axis.}
\item{drawTimeRange}{a boolean, set to \code{TRUE} to indicate the range of
    times in the top-left margin.}
\item{adorn}{optional \code{\link{expression}} to be performed immediately
    after drawing the panel. (See \code{\link{plot,adp-method}} for an example.)}
\item{fill}{boolean, set \code{TRUE} to fill the curve to zero (which it does
    incorrectly if there are missing values in \code{y}).}
\item{xlab}{name for x axis; defaults to \code{""}.}
\item{ylab}{name for y axis; defaults to the plotted item.}
\item{xaxs}{control x axis ending; see \code{\link{par}("xaxs")}.}
\item{yaxs}{control y axis ending; see \code{\link{par}("yaxs")}.}
\item{cex}{size of labels on axes; see \code{\link[graphics]{par}}("cex").}
\item{cex.axis}{see \code{\link[graphics]{par}}("cex.axis").}
\item{cex.main}{see \code{\link[graphics]{par}}("cex.main").}
\item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and also for
    \code{par(mar)}, computed from this.  The default is tighter than the R
    default, in order to use more space for the data and less for the axes.}
\item{mar}{value to be used with \code{\link{par}("mar")} to set margins.  THe
    default value uses significantly tighter margins than is the norm in R,
    which gives more space for the data.  However, in doing this, the existing
    \code{par("mar")} value is ignored, which contradicts values that may have
    been set by a previous call to \code{\link{drawPalette}}.  To get plot with
    a palette, first call \code{\link{drawPalette}}, then call \code{oce.plot.ts}
    with \code{mar=par("mar")}.}
\item{main}{title of plot.}
\item{despike}{boolean flag that can turn on despiking with
    \code{\link{despike}}.}
\item{axes}{boolean, set to \code{TRUE} to get axes plotted}
\item{tformat}{optional format for labels on the time axis}
\item{marginsAsImage}{boolean indicatingn whether to set the right-hand margin
    to the width normally taken by an image drawn with \code{\link{imagep}}.}
\item{grid}{if \code{TRUE}, a grid will be drawn for each panel.  (This
    argument is needed, because calling \code{\link{grid}} after doing a
    sequence of plots will not result in useful results for the individual
    panels.}
\item{grid.col}{colour of grid}
\item{grid.lty}{line type of grid}
\item{grid.lwd}{line width of grid}
\item{debug}{a flag that turns on debugging.  Set to 1 to get a
  moderate amount of debugging information, or to 2 to get more.}
\item{\dots}{graphical parameters passed down to \code{\link{plot}}.}
}

\details{Depending on the version of R, the standard \code{\link{plot}} and
\code{\link{plot.ts}} routines will not obey the time zone of the
data.  This routine gets around that problem.  It can also plot the
time range in the top-left margin, if desired; this string includes
the timezone, to remove any possible confusion.

The time axis is drawn with \code{\link{oce.axis.POSIXct}}.
}

\value{A list is silently returned, containing \code{xat} and \code{yat},
    values that can be used by \code{\link{oce.grid}} to add a grid to the
    plot.}

\examples{
library(oce)
t0 <- as.POSIXct("2008-01-01", tz="UTC")
t <- seq(t0, length.out=48, by="30 min")
y <- sin(as.numeric(t - t0) * 2 * pi / (12 * 3600))
oce.plot.ts(t, y, type='l', xaxs='i')
}

\author{Dan Kelley}

\keyword{hplot}
