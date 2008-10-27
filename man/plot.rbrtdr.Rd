\name{plot.rbrtdr}

\alias{plot.rbrtdr}

\title{Plot RBR temperature-depth recorder data}

\description{Plot RBR temperature-depth recorder data}

\usage{
\method{plot}{rbrtdr}(x, \dots)
}
\arguments{
  \item{x}{A \code{cdt} object, e.g. as read by \code{\link{read.rbrtdr}}.}
  \item{\dots}{optional arguments passed to plotting functions. A common example is to set \code{df}, for use in \link{sw.N2} calculations.}
}

\details{Creates a two-panel plot, with timeseries of temperature and pressure.}

\seealso{rbrtdr data can be
  created with \code{\link{read.rbrtdr}}.
  Summaries of RBRTDR data are provided by \code{\link{summary.rbrtdr}}.}

\examples{
\dontrun{
library(oce)
...
}
}
\author{Dan Kelley}
\keyword{misc}
