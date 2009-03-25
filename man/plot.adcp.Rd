\name{plot.adcp}

\alias{plot.adcp}

\title{Plot ADCP data}

\description{Plot ADCP data.}

\usage{\method{plot}{adcp}(x, which=1:4, col=oce.colors.two(128), zlim, \dots)}

\arguments{
  \item{x}{A \code{cdt} object, e.g. as read by \code{\link{read.adcp}}.}
  \item{which}{list of desired plot types.  These are graphed in
    panels running down from the top of the page.  See
    \dQuote{Details} for the meanings of various values of
    \code{which}.}
  \item{col}{color palette to use.  The default
  \code{\link{oce.colors.two}} palette blends from blue for negative
     values, through white at zero, to red at positive values; see
     \dQuote{Details}.}
  \item{zlim}{a range to be used as the \code{zlim} parameter to the
    \code{\link{image}} call that is used to create the image.  If
    omitted, \code{zlim} is set for each panel individually, to
    encompass the data of the panel and to be centred around zero.
    (Centring on zero is convenient for colour palettes that place a
    neutral colour there; see \dQuote{Details}.)}
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Creates a multi-panel summary plot of data measured by an ADCP
  cast. The panels are controlled by the \code{which} argument.

  \itemize{ 
    
    \item For beamwise data, \code{which}=1 gives an image plot of beam
    1 as a function of time and distance from sensor, and \code{which}
    values of 2 to 4 yield similar plots for the other beams.

    \item For velocity data, \code{which}=1, 2, and 3 yield the x, y,
    and z components of velocity, and \code{which}=4 yields a velocity
    error estimate computed from all beams together.

    \item For both beamwise and velocity data, \code{which}=5 yields
    backscatter intensity.

}

A more colourful plot is provided by using a \code{\link{rainbow}}
palette for \code{col}, although interpreting these in a quantitative
way is tricker, without a color map.  See \dQuote{Examples} for a
comparison.

By default, \code{plot.adcp} uses a \code{zlim} value for the
\code{\link{image}} that is constructed to contain all the data, but to
be symmetric about zero.  This is done on a per-panel basis, and the
scale is plotted at the top-right corner, along with the name of the
variable being plotted. You may also supply \code{zlim} as one of the
\dots arguments, but be aware that a reasonable limit on horizontal
velocity components is unlikely to be of much use for the vertical
component.

}

\seealso{The data may be read with \code{\link{read.adcp}}. Conversion
  from beam to velocity values can be done with
  \code{\link{adcp.beam2velo}}. Summaries of ADCP data are provided by
  \code{\link{summary.adcp}}.}

\examples{
\dontrun{
library(oce)
d <- read.oce(file = "~/SL08F001.000", skip = 5000, read = 6 * 30)
e <- adcp.beam2velo(d)
plot(e, which=1:3)

par(mfrow=c(2,1))
plot.adcp(e, which=3)
plot.adcp(e, which=3, col=rainbow(256))
}
}
\author{Dan Kelley}
\keyword{misc}
