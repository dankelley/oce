\name{plotScan}

\alias{plotScan}

\title{Plot seawater data in a low-level fashion}

\description{Plot CTD data as time-series against scan number, to help
  with trimming extraneous data from a CTD cast.}

\usage{plotScan(x, which=1, type='l',
    mgp=getOption("oceMgp"),
    mar=c(mgp[1]+1.5,mgp[1]+1.5,mgp[1],mgp[1]), \dots)}

\arguments{
  \item{x}{A \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{which}{A vector containing numerical codes specifying the panels to
      draw: 1 for pressure vs scan, 2 for \code{diff(pressure)} vs scan, 3 for
      temperature vs scan, and 4 for salinity vs scan.}
  \item{type}{line type}
  \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
      also for \code{par(mar)}, computed from this.  The default is
      tighter than the R default, in order to use more space for the data
      and less for the axes.}
  \item{mar}{value to be used with \code{\link{par}("mar")}.  If set to
      \code{NULL}, then \code{par("mar")} is used.  A good choice for a TS
      diagram with a palette to the right is \code{mar=par("mar")+c(0, 0, 0,
          1))}.} 
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Plots \code{ctd} data as time-series against the scan number, as an
    aide to trimming to downcasts, \emph{etc}.  }

\seealso{
  \code{\link{summary.ctd}} summarizes a \code{ctd} object
  \code{\link{plot.ctd}} plot summary diagram of \code{ctd} object.
  \code{\link{read.ctd}} scans \code{ctd} object from a file.
}

\examples{
library(oce)
data(ctdRaw) 
plotScan(ctdRaw)
abline(v=c(130, 350), col='red') # useful for ctdTrim()
}

\author{Dan Kelley}

\keyword{hplot}
