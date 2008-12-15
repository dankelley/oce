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

\details{Several plots are available: \code{which}=1 for a timeseries
  plot of temperature, \code{which}=2 for a timeseries plot of pressure,
  \code{which}=3 for a scatterplot of the temperature profile, and
  \code{which}=4 for textual information about the dataset.  Supply a
  list of \code{which} values, to get multi-panel plots.}

\seealso{See \code{\link{read.rbrtdr}} for more information on dealing
  with the \code{rbrtdr} data type, including examples..}

\author{Dan Kelley}
\keyword{misc}
