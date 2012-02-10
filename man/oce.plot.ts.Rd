\name{oce.plot.ts}
\alias{oce.plot.ts}
\encoding{UTF-8}
\title{Plot a time-series, obeying the timezone}

\description{Plot a time-series, obeying the timezone and possibly drawing the
    range in the top-left margin}

\usage{
oce.plot.ts(x, y, type="l", xlim, ylim, xlab="", ylab="", 
            drawTimeRange=TRUE, adorn=NULL, fill=FALSE,
            xaxs="i", yaxs="i",
            cex=par("cex"), cex.axis=par("cex.axis"), cex.main=par("cex.main"),
            mgp=getOption("oceMgp"),
            mar=c(mgp[1]+if(nchar(xlab)>0) 1.5 else 1, mgp[1]+1.5, mgp[2]+1, mgp[2]+3/4),
            mai.palette=c(0, 1/8, 0, 3/8),
            main="",
            despike=FALSE,
            axes=TRUE,
            marginsAsImage=FALSE,
            grid=FALSE, grid.col="darkgray", grid.lty="dotted", grid.lwd=1,
            debug=getOption("oceDebug"),
            \dots)}

\arguments{
\item{x}{the times of observations.}
\item{y}{the observations.}
\item{type}{plot type, \code{"l"} for lines, \code{"p"} for points.}
\item{xlim}{optional limit for x axis.}
\item{ylim}{optional limit for y axis.}
\item{drawTimeRange}{a boolean, set to \code{TRUE} to indicate the range of times 
  in the top-left margin.}
\item{adorn}{optional \code{\link{expression}} to be performed
  immediately after drawing the panel. (See \code{\link{plot.adp}} for
  an example.)}
\item{fill}{boolean, set \code{TRUE} to fill the curve to zero (which it
  does incorrectly if there are missing values in \code{y}).}
\item{xlab}{names for x axis.}
\item{ylab}{names for y axis.}
\item{xaxs}{character indicating whether image should extend to edge
  of x axis (with value \code{"i"}) or not; see
  \code{\link[graphics]{par}}("xaxs").}
\item{yaxs}{as \code{xaxs} but for y axis.}
\item{cex}{size of labels on axes; see \code{\link[graphics]{par}}("cex").}
\item{cex.axis}{see \code{\link[graphics]{par}}("cex.axis").}
\item{cex.main}{see \code{\link[graphics]{par}}("cex.main").}
\item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
  also for \code{par(mar)}, computed from this.  The default is
  tighter than the R default, in order to use more space for the
  data and less for the axes.}
\item{mar}{value to be used with \code{\link{par}("mar")}.}
\item{mai.palette}{margins, in inches, for the palette}
\item{main}{title of plot.}
\item{despike}{boolean flag that can turn on despiking with \code{\link{despike}}.}
\item{grid}{if \code{TRUE}, a grid will be drawn for each panel.  (This
    argument is needed, because calling \code{\link{grid}} after doing a
    sequence of plots will not result in useful results for the individual
    panels.}
\item{grid.col}{color of grid}
\item{grid.lty}{line type of grid}
\item{grid.lwd}{line width of grid}
\item{debug}{a flag that turns on debugging.  Set to 1 to get a
  moderate amount of debugging information, or to 2 to get more.}
\item{axes}{boolean, set to \code{TRUE} to get axes plotted}
\item{marginsAsImage}{FALSE},
\item{\dots}{graphical parameters passed down to \code{\link{plot}}.}
}

\details{Depending on the version of R, the standard \code{\link{plot}} and
\code{\link{plot.ts}} routines will not obey the time zone of the
data.  This routine gets around that problem.  It can also plot the
time range in the top-left margin, if desired; this string includes
the timezone, to remove any possible confusion.

The time axis is drawn with \code{\link{oce.axis.POSIXct}}.
}

\examples{
library(oce)
t.start <- as.POSIXct("2008-01-01", tz="UTC")
t <- seq(t.start, length.out=48, by="30 min")
y <- sin(as.numeric(t - t.start) * 2 * pi / (12 * 3600))
oce.plot.ts(t, y, type='l', xaxs='i')

\dontrun{
weatherplot <- function(id=6358, time=as.POSIXlt(Sys.Date()))
{
    ## The default stationID is Halifax, NS, Canada
    site <- "http://www.climate.weatheroffice.gc.ca/climateData/bulkdata_e.html"
    time <- as.POSIXlt(time)
    f <- paste(site,
               "?timeframe=1&StationID=", id,
               "&&Year=", time$year+1900,
               "&Month=", time$mon+1,
               "&Day=", time$mday,
               "&format=csv", sep="")
    ## Read lines first, to see where header ends.  Cannot parse header, owing
    ## to multi-byte strings there.
    ll <- readLines(f)
    skip <- which(grepl("Date", ll, perl=TRUE))
    d <- read.csv(f, skip=skip, header=FALSE)
    t <- as.POSIXct(d[,1])
    p <- d[,19]
    oce.plot.ts(t, p, type='l', ylab="Pressure [dbar]", cex=2/3, main=paste("Station", id))
}
weatherplot()
}
}

\author{Dan Kelley}

\keyword{hplot}
