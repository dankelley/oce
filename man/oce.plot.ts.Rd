\name{oce.plot.ts}
\alias{oce.plot.ts}
\encoding{UTF-8}
\title{Plot a time-series, obeying the timezone}

\description{Plot a time-series, obeying the timezone and possibly drawing the range in the top-left margin}

\usage{
oce.plot.ts(x, y, draw.time.range=TRUE, xaxs="r", grid=TRUE, adorn=NULL,
  fill=FALSE,
  xlab="", ylab="", 
  cex=par("cex"), cex.axis=par("cex.axis"), cex.main=par("cex.main"),
  mgp=getOption("oce.mgp"),
  mar=c(mgp[1]+if(nchar(xlab)>0) 1 else 0.5,
  mgp[1]+if(nchar(ylab)>0) 1.5 else 1, mgp[2]+1, mgp[2]+3/4),
  type="l",
  main="",
  despike=FALSE,
  debug=getOption("oce.debug"),
  \dots)}

\arguments{
\item{x}{the times of observations.}
\item{y}{the observations.}
\item{draw.time.range}{a boolean, set to \code{TRUE} to indicate the range of times 
  in the top-left margin.}
\item{xaxs}{character indicating whether image should extend to edge
  of x axis (with value \code{"i"}) or not; see
  \code{\link[graphics]{par}}("xaxs").}
\item{grid}{boolean, set to \code{TRUE} to get a grid on the plot.  (Note that
  \code{\link{grid}} does work correctly because it does not account
  properly for tic locations on the time axis.}
\item{adorn}{optional \code{\link{expression}} to be performed
  immediately after drawing the panel. (See \code{\link{plot.adp}} for
  an example.)}
\item{fill}{boolean, set \code{TRUE} to fill the curve to zero (which it
  does incorrectly if there are missing values in \code{y}).}
\item{xlab}{names for x axis.}
\item{ylab}{names for y axis.}
\item{cex}{size of labels on axes; see \code{\link[graphics]{par}}("cex").}
\item{cex.axis}{see \code{\link[graphics]{par}}("cex.axis").}
\item{cex.main}{see \code{\link[graphics]{par}}("cex.main").}
\item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
  also for \code{par(mar)}, computed from this.  The default is
  tighter than the R default, in order to use more space for the
  data and less for the axes.}
\item{mar}{value to be used with \code{\link{par}("mar")}.}
\item{type}{plot type.}
\item{main}{title of plot.}
\item{despike}{boolean flag that can turn on despiking with \code{\link{despike}}.}
\item{debug}{a flag that turns on debugging.  Set to 1 to get a
  moderate amount of debugging information, or to 2 to get more.}
\item{\dots}{graphical parameters passed down to \code{\link{plot}}.}
}

\details{Depending on the version of R, the standard \code{\link{plot}} and
\code{\link{plot.ts}} routines will not obey the time zone of the
data.  This routine gets around that problem.  It can also plot the
time range in the top-left margin, if desired; this string includes
the timezone, to remove any possible confusion.

For flexibility, this does not set to line-type plots; use \code{type='l'} to get these.
}

\examples{
library(oce)
t.start <- as.POSIXct("2008-01-01", tz="UTC")
t <- seq(t.start, length.out=48, by="30 min")
y <- sin(as.numeric(t - t.start) * 2 * pi / (12 * 3600))
oce.plot.ts(t, y, type='l', xaxs='i')

## weather plot
\dontrun{
library(oce)
weatherplot <- function(stationID=6358, time=as.POSIXlt(Sys.Date()), title)
{
    ## The default stationID is Halifax, NS, Canada
    site <- "http://www.climate.weatheroffice.gc.ca/climateData/bulkdata_e.html"
    time <- as.POSIXlt(time)
    f <- paste(site,
               "?StationID=", stationID,
               "&Year=", time$year+1900,
               "&Month=", time$mon+1,
               "&Day=", time$mday,
               "&format=csv&type=hly", sep="")
    d <- read.csv(f, skip=15, header=TRUE)
    oce.plot.ts(as.POSIXct(d$Date.Time), d$Stn.Press..kPa., type='l',
                ylab="Pressure [dbar]", cex=2/3,
                main=if(missing(title)) stationID else title)
}
weatherplot(6358, time=as.POSIXlt(Sys.Date()), title="Halifax NS")
}
}

\author{Dan Kelley}

\keyword{misc}
