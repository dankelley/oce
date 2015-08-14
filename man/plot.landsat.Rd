\name{plot.landsat}

\alias{plot.landsat}
\alias{plot,landsat,missing-method}
\alias{plot,landsat-method}

\title{Plot landsat data}

\description{Plot landsat data.}

\usage{\S4method{plot}{landsat}(x, band, which=1, decimate=TRUE, zlim, utm=FALSE,
    col=oce.colorsPalette, drawPalette=TRUE, showBandName=TRUE,
    alpha.f=1, red.f=2, green.f=2, blue.f=4,
    offset=c(0, 0, 0, 0),
    transform=diag(c(red.f, green.f, blue.f, alpha.f)),
    debug=getOption("oceDebug"), \dots)
}
\arguments{

  \item{x}{A \code{landsat} object, e.g. as read by
      \code{\link{read.landsat}}.}

  \item{band}{if given, the name of the band.  For Landsat-8 data, this may be
      one of: \code{"aerosol"}, \code{"blue"}, \code{"green"}, \code{"red"},
      \code{"nir"}, \code{"swir1"}, \code{"swir2"}, \code{"panchromatic"},
      \code{"cirrus"}, \code{"tirs1"}, or \code{"tirs2"}.  For Landsat-7 data,
      this may be one of \code{"blue"}, \code{"green"}, \code{"red"},
      \code{"nir"}, \code{"swir1"}, \code{"tirs1"}, \code{"tirs2"},
      \code{"swir2"}, or \code{"panchromatic"}.  For Landsat data prior to
      Landsat-7, this may be one of \code{"blue"}, \code{"green"},
      \code{"red"}, \code{"nir"}, \code{"swir1"}, \code{"tirs1"},
      \code{"tirs2"}, or \code{"swir2"}.  If \code{band} is not given, the
      (\code{"tirs1"}) will be used if it exists in the object data, or
      otherwise the first band will be used.  In addition to the above, using
      \code{band="temperature"} will plot an estimate of at-satellite
      brightness temperature, computed from the \code{tirs1} band, and
      \code{band="terralook"} will plot a sort of natural colour; see
      \dQuote{Details}.}

  \item{which}{desired plot type; 1=image, 2=histogram.}

  \item{decimate}{decimation passed to \code{\link{imagep}} for image plots.
      The default yields faster plotting.  Some decimation is sensible for
      full-size images, since no graphical displays can show 16 thousand pixels
      on a side.}

   \item{zlim}{either a pair of numbers giving the limits for the colourscale,
      or \code{"histogram"} to have a flattened histogram (i.e. to maximally
      increase contrast throughout the domain.)  If not given, the 1 and 99
      percent quantiles are calculated and used as limits.}

  \item{utm}{logical indicating whether to use UTS (easting and northing) instead
      of longitude and latitude on plot}

  \item{col}{either a function yielding colours, taking a single integer
      argument with the desired number of colours, or the string
      \code{"natural"}, which combines the information in the \code{red},
      \code{green} and \code{blue} bands and produces a natural-hue image.  In
      the latter case, the band designation is ignored, and the object must
      contain the three colour bands.}

  \item{drawPalette}{indication of the type of palette to draw, if
    any. See \code{\link{imagep}} for details.}

  \item{showBandName}{if \code{TRUE}, the band name is plotted in the top
      margin, near the right-hand side.}

  \item{alpha.f}{argument used if \code{col="natural"}, to adjust colours
      with \code{\link{adjustcolor}}}

  \item{red.f}{argument used if \code{col="natural"}, to adjust colours with
      \code{\link{adjustcolor}}.  Higher values of \code{red.f} cause red hues
      to be emphasized (e.g. dry land).}

  \item{green.f}{argument used if \code{col="natural"}, to adjust colours with
      \code{\link{adjustcolor}}.  Higher values of \code{green.f} emphasize
      green hues (e.g. forests).}

  \item{blue.f}{argument used if \code{col="natural"}, to adjust colours with
      \code{\link{adjustcolor}}.  Higher values of \code{blue.f} emphasize blue
      hues (e.g. ocean).}

  \item{offset}{argument used if \code{col="natural"}, to adjust colours with
      \code{\link{adjustcolor}}}

  \item{transform}{argument used if \code{col="natural"}, to adjust colours
      with \code{\link{adjustcolor}}}

  \item{debug}{set to a positive value to get debugging information during
      processing.}

  \item{\dots}{optional arguments passed to plotting functions.}

}

\details{Since landsat images are very detailed, it is sensible to use a fast
    plotting device, e.g. \code{\link{x11}}, and to set values of
    \code{decimate} appropriate to the data and plotting device.
  
    Using \code{zlim="histogram"} is probably the fastest way to explore an
    image for detail, but it is important to bear in mind that it yields a
    nonlinear colourscale.

    The histogram plot can be handy in setting scales, e.g. when an image has a
    fair bit of land, the histogram will be double-lobed, and so quick
    examination can help in setting a good value for \code{zlim} to emphasize
    the water or the land.  If \code{breaks} is present in \code{\dots} then
    its value is used in the call to \code{\link{hist}}; if not, 100 is used.


    Setting \code{band="temperature"} plots an estimate of temperature,
    calculated as explained in the documentation for
    \code{\link{landsat-class}}.

    Setting \code{band="terralook"} creates a colour scale that tends to make
    the ocean blue and forests green.  This uses the USGS ``terralook'' method
    (reference 1) of combining the \code{red}, \code{green} and \code{nir}
    bands, producing new pseudo colors defined as \code{R=red},
    \code{G=2/3*green+1/3*nir} and \code{B=2/3*green-1/3*nir}.  For obvious
    reasons, this requires that the image contains these three channels.  If
    \code{band="terralook"} the \code{col} argument is ignored, because an
    internal colour scheme is used.  Still, this scheme may be altered through
    the use of the arguments \code{red.f}, etc.; their action is described in
    the documentation for \code{\link{adjustcolor}} but users can get good
    results just by altering the default values and seeing the results, e.g.
    increasing \code{blue.f} makes water bluer.}

\seealso{The documentation for \code{\link{landsat-class}} explains the
    structure of landsat objects, and also outlines the other functions dealing
    with them.  The \CRANpkg{ocedata} package provides a dataset named
    \code{landsat}.}

\references{1. \url{http://terralook.cr.usgs.gov/what_is_terralook.php}}

\examples{
\dontrun{
library(oce)
library(ocedata)
data(landsat)
plot(landsat, band="temperature")
plot(landsat, which=2, band="temperature", breaks=100)
}
}

\author{Dan Kelley}

\keyword{hplot}
