\name{plot.echosounder}

\alias{plot.echosounder}
\alias{plot,echosounder,missing-method}
\alias{plot,echosounder-method}

\title{Plot echosounder data}

\description{Plot echosounder data}

\usage{\S4method{plot}{echosounder}(x,
     which=1, beam="a",
     newx,
     xlab, ylab,
     xlim, ylim, zlim,
     type="l", col=oce.colorsJet, lwd=2,
     despike=FALSE,
     drawBottom, ignore=5,
     drawTimeRange=FALSE, drawPalette=TRUE,
     radius, coastline,
     adorn=NULL,
     mgp=getOption("oceMgp"),
     mar=c(mgp[1]+1,mgp[1]+1,mgp[1]+1,mgp[1]+1),
     atTop, labelsTop,
     tformat,
     debug=getOption("oceDebug"),
     \dots)
}
\arguments{

  \item{x}{An \code{echosounder} object, e.g. as read by
      \code{\link{read.echosounder}}, or created by
      \code{\link{as.echosounder}}.}

  \item{which}{list of desired plot types: \code{which=1} or \code{which="zt
          image"} gives a z-time image, \code{which=2} or \code{which="zx
          image"} gives a z-distance image, and \code{which=3} or
      \code{which="map"} gives a map showing the cruise track.  In the image
      plots, the display is of \code{\link{log10}} of amplitude, trimmed to
      zero for any amplitude values less than 1 (including missing values,
      which equal 0).  Add 10 to the numeric codes to get the secondary data
      (non-existent for single-beam files, }

  \item{beam}{a more detailed specification of the data to be plotted.  For
      single-beam data, this may only be \code{"a"}.  For dual-beam data, this
      may be \code{"a"} for the narrow-beam signal, or \code{"b"} for the
      wide-beam signal.  For split-beam data, this may be \code{"a"} for
      amplitude, \code{"b"} for x-angle data, or \code{"c"} for y-angle data.}

  \item{newx}{optional vector of values to appear on the horizontal axis if
      \code{which=1}, instead of time.  This must be of the same length as the
      time vector, because the image is remapped from time to \code{newx} using
      \code{\link{approx}}.}

  \item{xlab, ylab}{optional labels for the horizontal and vertical axes; if
      not provided, the labels depend on the value of \code{which}.}

  \item{xlim}{optional range for x axis.}

  \item{ylim}{optional range for y axis.}

  \item{zlim}{optional range for colour scale.}

  \item{type}{type of graph, \code{"l"} for line, \code{"p"} for points, or
      \code{"b"} for both.}

  \item{col}{colour scale for image, a function}

  \item{lwd}{line width (ignored if \code{type="p"})}

  \item{atTop}{optional vector of time values, for labels at the top of the
      plot produced with \code{which=2}.  If \code{labelsTop} is provided, then
      it will hold the labels.  If \code{labelsTop} is not provided, the labels
      will be constructed with the \code{\link{format}} function, and these may
      be customized by supplying a \code{format} in the \dots arguments.}

  \item{labelsTop}{optional vector of character strings to be plotted above the
      \code{atTop} times.  Ignored unless \code{atTop} was provided.}

  \item{tformat}{optional argument passed to \code{\link{imagep}}, for plot
      types that call that function.  (See \code{\link{strptime}} for the
      format used.)}

  \item{despike}{remove vertical banding by using \code{\link{smooth}} to smooth
      across image columns, row by row.}

  \item{drawBottom}{optional flag used for section images.  If \code{TRUE},
      then the bottom is inferred as a smoothed version of the ridge of highest
      image value, and data below that are grayed out after the image is drawn.
      If \code{drawBottom} is a colour, then that colour is used, instead of
      white.  The bottom is detected with \code{\link{findBottom}}, using the
      \code{ignore} value described next.}

  \item{ignore}{optional flag specifying the thickness in metres of a surface
      region to be ignored during the bottom-detection process.  This is
      ignored unless \code{drawBottom=TRUE}.}

  \item{drawTimeRange}{if \code{TRUE}, the time range will be drawn at the top.
      Ignored except for \code{which=2}, i.e. distance-depth plots.}

  \item{drawPalette}{if \code{TRUE}, the palette will be drawn.}
 
  \item{radius}{radius to use for maps; ignored unless \code{which=3} or
      \code{which="map"}.}

  \item{coastline}{coastline to use for maps; ignored unless \code{which=3} or
      \code{which="map"}.}

  \item{adorn}{list of expressions to be executed for the panels in turn, e.g.
      to adorn the plots.  If the number matches the number of panels, then the
      strings are applied to the appropriate panels, as they are drawn from
      top-left to bottom-right.   If only a single expression is provided, it
      is used for all panels.  (See \dQuote{Examples}.)} \item{mgp}{3-element
      numerical vector to use for \code{par(mgp)}, and also for
      \code{par(mar)}, computed from this.  The default is tighter than the R
      default, in order to use more space for the data and less for the axes.}

  \item{mar}{value to be used with \code{\link{par}("mar")}.} \item{debug}{set
      to an integer exceeding zero, to get debugging information during
      processing.}

  \item{\dots}{optional arguments passed to plotting functions.  For example,
      for maps, it is possible to specify the radius of the view in kilometres,
      with \code{radius}.}

}

\details{Simple linear approximation is used when a \code{newx} value is
    specifie with the \code{which=2} method, but arguably a gridding method
    should be used, and this may be added in the future.}

\seealso{The documentation for \code{\link{echosounder-class}} explains the
    structure of echosounder objects, and also outlines the other functions
    dealing with them.}

\value{A list is silently returned, containing \code{xat} and \code{yat},
    values that can be used by \code{\link{oce.grid}} to add a grid to the
    plot.}

\examples{
\dontrun{
library(oce)
data(echosounder)
plot(echosounder, which=c(1,2), drawBottom=TRUE)
}
}

\author{Dan Kelley, with extensive help from Clark Richards}

\keyword{hplot}
