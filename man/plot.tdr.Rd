\name{plot.tdr}

\alias{plot.tdr}

\title{Plot TDR temperature-depth recorder data}

\description{Plot TDR temperature-depth recorder data}

\usage{\method{plot}{tdr}(x, which=1:4, adorn=NULL, \dots)}

\arguments{
  \item{x}{\code{cdt} object, typically result of \code{\link{read.tdr}}.}
  \item{which}{if a subset of the plots is required, specify a subset of
    the numbers 1:4.  These are codes for the panels, which will be
    drawn from top-left to bottom-right (see Details).}
  \item{adorn}{list of expressions to be executed for the panels in
    turn, e.g. to adorn the plots.  If the number matches the number
    of panels, then the strings are applied to the appropriate panels,
    as they are drawn from top-left to bottom-right.   If only a single
    expression is provided, it is used for all panels.  (See
    \dQuote{Examples}.)}
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Several plots are available: \code{which}=1 for a timeseries
  plot of temperature, 
  \code{which}=2 for textual information about the dataset,
  \code{which}=3 for a timeseries plot of pressure,
  and
  \code{which}=4 for a scatterplot of the temperature profile, .  You
  may supply a list of \code{which} values, to get multi-panel plots.}

\examples{
data(tdr)
plot(tdr, which=c(1,3), 
    adorn=expression({abline(v=as.POSIXct('2008-07-04 20:00:00',tz='UTC'),col='red')}))
}

\seealso{See \code{\link{read.tdr}} for more information on dealing
  with the \code{tdr} data type, including examples.}

\author{Dan Kelley}
\keyword{misc}
