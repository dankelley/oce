\name{plot.pt}

\alias{plot.pt}

\title{Plot pT (pressure-temperature) recorder data}

\description{Plot pT temperature-depth recorder data}

\usage{\method{plot}{pt}(x, which=1:4, title=deparse(substitute(x)), adorn=NULL,
     mgp=getOption("oce.mgp"), \dots)}

\arguments{
  \item{x}{\code{pt} object, typically result of \code{\link{read.pt}}.}
  \item{which}{if a subset of the plots is required, specify a subset of
    the numbers 1:4.  These are codes for the panels, which will be
    drawn from top-left to bottom-right (see Details).}
  \item{title}{character string to be used in the text-summary panel
    (\code{which}=2)}
  \item{adorn}{list of expressions to be executed for the panels in
    turn, e.g. to adorn the plots.  If the number matches the number
    of panels, then the strings are applied to the appropriate panels,
    as they are drawn from top-left to bottom-right.   If only a single
    expression is provided, it is used for all panels.  (See
    \dQuote{Examples}.)}
  \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
    also for \code{par(mar)}, computed from this.  The default is
    tighter than the R default, in order to use more space for the data
    and less for the axes.}
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
data(pt)
plot(pt, which=c(1,3), 
    adorn=expression({abline(v=as.POSIXct('2008-07-04 20:00:00',tz='UTC'),col='red')}))
}

\seealso{See \code{\link{read.pt}} for more information on dealing
  with the \code{pt} data type, including examples.}

\author{Dan Kelley}
\keyword{misc}
