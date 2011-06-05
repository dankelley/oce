\name{plot.lobo}

\alias{plot.lobo}

\title{Plot lobo data}

\description{Plot a summary diagram for lobo data.}

\usage{\method{plot}{lobo}(x, 
     adorn=NULL,
     mgp=getOption("oceMgp"), 
     mar=c(mgp[2]+1, mgp[1]+1, 1, mgp[1]+1.25),
     \dots)}

\arguments{
  \item{x}{A \code{lobo} object, e.g. as read by \code{\link{read.lobo}}.}
  \item{adorn}{list of expressions to be executed for the panels in
    turn, e.g. to adorn the plots.  If the number matches the number 
    of panels, then the strings are applied to the appropriate panels,
    as they are drawn from top-left to bottom-right.   If only a single
    expression is provided, it is used for all panels. (See
    \dQuote{Examples}.)}
  \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
    also for \code{par(mar)}, computed from this.  The default is
    tighter than the R default, in order to use more space for the data
    and less for the axes.}
  \item{mar}{value to be used with \code{\link{par}("mar")}.}
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Creates a summary plot for a lobo dataset.}

\value{None.}

\seealso{
  \code{\link{summary.lobo}} summarizes the information, while
  \code{\link{read.lobo}} scans it from a file.
}

\examples{
library(oce)
data(lobo)
plot(lobo)
# illustrate adornment
plot(lobo,
     adorn=expression({abline(v=as.POSIXct("2009-03-23 00:00:00"), col="red")}))
}
\references{\url{http://lobo.satlantic.com/} and
\url{http://www.mbari.org/lobo/}
}

\author{Dan Kelley}

\keyword{hplot}
