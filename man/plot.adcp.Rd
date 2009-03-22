\name{plot.adcp}

\alias{plot.adcp}

\title{Plot ADCP data}

\description{Plot ADCP data.}

\usage{\method{plot}{adcp}(x, which=1:4, \dots)}

\arguments{
  \item{x}{A \code{cdt} object, e.g. as read by \code{\link{read.adcp}}.}
  \item{which}{list of desired plot types.  These are graphed in panels
    running down from the top of the page.  
    Which=1 gives an image plot of beam 1 as a function of time and
    distance from sensoe.  Which=2 to 4 gives similar plots, for the
    other beams.}
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Creates a multi-panel summary plot of data measured by an ADCP
  cast. The panels are controlled by the \code{which} argument.
}

\seealso{The data may be read with \code{\link{read.adcp}}. Conversion
  from beam to velocity values can be done with
  \code{\link{adcp.beam2velo}}. Summaries of ADCP data are provided by
  \code{\link{summary.adcp}}.}

\examples{
\dontrun{
library(oce)
d <- read.oce("~/SL08F001.000",read=60)
summary(d)
plot(d, col=cm.colors(128))
}
}
\author{Dan Kelley}
\keyword{misc}
