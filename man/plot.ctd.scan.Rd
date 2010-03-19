\name{plot.ctd.scan}

\alias{plot.ctd.scan}

\title{Plot seawater data in a low-level fashion}

\description{Plot CTD data as time-series against scan number, to help
  with trimming extraneous data from a CTD cast.}

\usage{plot.ctd.scan(x, name = "scan", 
 S.col = "darkgreen", T.col= "darkred", p.col = "blue",
 adorn=NULL, mgp=getOption("oce.mgp"), \dots)
}

\arguments{
  \item{x}{A \code{cdt} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{name}{name of variable for x axis}
  \item{S.col}{colour for salinity}
  \item{T.col}{colour for temperature}
  \item{p.col}{colour for pressure}
  \item{adorn}{list of character strings containing commands to be
    executed for the panels.  If the number matches the number of
    panels, then the strings are applied to the appropriate panels, as
    they are drawn from top-left to bottom-right.   If only a single
    string is provided, it is used for all panels.  (See
    \dQuote{Examples}.)}
  \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
    also for \code{par(mar)}, computed from this.  The default is
    tighter than the R default, in order to use more space for the data
    and less for the axes.}
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Plots \code{ctd} data as time-series against the scan number,
  as an aide to trimming to downcasts, \emph{etc}.  }

\seealso{
  \code{\link{summary.ctd}} summarizes a \code{ctd} object
  \code{\link{plot.ctd}} plot summary diagram of \code{ctd} object.
  \code{\link{read.ctd}} scans \code{ctd} object from a file.
}

\examples{
library(oce)
data(ctd) 
plot.ctd.scan(ctd)

# demonstrate adding elements to plots, e.g. to narrow
# down on good start end points
plot.ctd.scan(ctd, adorn="abline(v=240,col='red')")
}

\author{Dan Kelley}

\keyword{misc}
