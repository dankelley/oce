\name{plot.ctd.scan}

\alias{plot.ctd.scan}

\title{Plot seawater data in a low-level fashion}

\description{Plot CTD data as time-series against scan number, to help
  with trimming extraneous data from a CTD cast.}

\usage{plot.ctd.scan(x, name = "scan", 
 S.col = "darkgreen", T.col= "darkred", p.col = "blue", close.screens=TRUE, \dots)
}

\arguments{
  \item{x}{A \code{cdt} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{name}{name of variable for x axis}
  \item{S.col}{colour for salinity}
  \item{T.col}{colour for temperature}
  \item{p.col}{colour for pressure}
  \item{close.screens}{boolean, indicating whether to this function
    should use \code{\link{oce.close.screen}} to close the drawing
    screens after plotting.  Set this to \code{FALSE} in order to
    leave the screens open, enabling you to use
    \code{\link{oce.screen}} (with a\code{new=FALSE}) to add new items
    to the panels (see cautions at \code{\link{oce.split.screen}}).
    Note that \code{\link{oce.close.screen}} should be called after
    doing any such additions.}
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
plot.ctd.scan(ctd, close.screens=FALSE)
oce.screen(1,FALSE)
abline(v=240,col='red')
}

\author{Dan Kelley}

\keyword{misc}
