\name{plot.tide}
\alias{plot.tide}
\title{Plot a tidal fit}
\description{
Plot a summary diagram for a tidal fit.
}
\usage{
\method{plot}{tide}(x, label.if=NULL, style=c("spikes", "staircase"), log="", ...)
}
\arguments{
  \item{x}{A \code{tide} object, as created by \code{\link{fit.tide}}.}
  \item{label.if}{if NULL, the function will indicate some particular tidal constituents; if a value is provided, labels will be given for any constituent with amplitude exceeding the value provided.}
  \item{style}{the style for the plot, either as spikes or as a staircase of cumulative spectral energy}
  \item{log}{if set to \dQuote{\code{x}}, the frequency axis will be logarithmic.}
  \item{\dots}{optional arguments passed to plotting functions.}
}
\details{
Creates a plot for a \dQuote{\code{tide}} object.
}
\value{
  None.
}
\seealso{
\code{\link{fit.tide}} fits a \dQuote{\code{tide}} object, and
\code{\link{summary.tide}} summarizes such an object.
}
\examples{
library(oce)
data(sealevel)
# tide <- fit.tide(sealevel)
# plot(tide)
}
%\references{}
\author{Dan Kelley}
\keyword{misc}
