\name{plot.tdr}

\alias{plot.tdr}

\title{Plot TDR temperature-depth recorder data}

\description{Plot TDR temperature-depth recorder data}

\usage{\method{plot}{tdr}(x, which=1:4, \dots)}

\arguments{
  \item{x}{\code{cdt} object, typically result of \code{\link{read.tdr}}.}
  \item{which}{if a subset of the plots is required, specify a subset of
    the numbers 1:4 (see Details).}
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Several plots are available: \code{which}=1 for a timeseries
  plot of temperature, \code{which}=2 for a timeseries plot of pressure,
  \code{which}=3 for textual information about the dataset and
  \code{which}=4 for a scatterplot of the temperature profile, .  You
  may supply a list of \code{which} values, to get multi-panel plots.

  Note that if a single-panel plot is made, then \code{\link{par}} is
  not adjusted.  This means that you may add to the plot easily, which
  proves to be useful in determining in-water time intervals (see
  \code{\link{read.tdr}} for an example).}

\seealso{See \code{\link{read.tdr}} for more information on dealing
  with the \code{tdr} data type, including examples.}

\author{Dan Kelley}
\keyword{misc}
