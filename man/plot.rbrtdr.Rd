\name{plot.rbrtdr}

\alias{plot.rbrtdr}

\title{Plot RBR temperature-depth recorder data}

\description{Plot RBR temperature-depth recorder data}

\usage{\method{plot}{rbrtdr}(x, which=1:4, \dots)}

\arguments{
  \item{x}{\code{cdt} object, typically result of \code{\link{read.rbrtdr}}.}
  \item{which}{if a subset of the plots is required, specify a subset of
    the numbers 1:4 (see Details).}
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Three plots (selectable by \code{which}) are available:
  which=1 for a timeseries plot of temperature, which=2 for a
  timeseries plot of pressure, which=3 for a scatterplot of the
  temperature profile, and which=4 for textual information about the
  dataset.}

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
