\name{plot.coastline}
\alias{plot.coastline}
\title{Plot a coastline}
\description{
  Plot a coastline.
}
\usage{
\method{plot}{coastline}(x, asp=NA, \dots)
}
\arguments{
  \item{x}{A \code{coastline} object, e.g. as read by \code{\link{read.coastline}} or created by \code{\link{as.coastline}}.}
  \item{asp}{Aspect ratio for plot.  The default is for \code{plot.coastline} to set the aspect ratio to give natural latitude-longitude scaling at the average latitude in the data range.  In most cases, you will probably want to set \code{asp} yourself, e.g. coastal shapes near 45N will have the correct shape if you set \code{asp=1/cos(45*pi/180)}.  Setting \code{asp=1} makes sense for world views.}
  \item{\dots}{optional arguments passed to plotting functions.  For example, set \code{yaxp=c(-90,90,4)} for a plot extending from pole to pole.}
}
\details{
  Plots a coastline.
}
\value{
  None.
}
\seealso{
\code{\link{read.coastline}} can read coastlines from data files, provided that
they are in a certain class of formats.  A coastline may be summarized by \code{\link{summary.coastline}}.
}
\examples{
library(oce)
data(coastline.world)
plot(coastline.world)
}
\references{
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}
