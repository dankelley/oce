\name{imagep}
\alias{imagep}
\title{Plot an image with a colour palette}
\description{Plot an image with a colour palette}
\usage{imagep(x, y, z, 
      xlim, ylim, zlim, 
      flip.y=FALSE,
      xlab="", ylab="", zlab="",
      breaks, col,
      do.layout=TRUE,
      draw.contours=TRUE,
      draw.time.range=TRUE,
      mgp=getOption("oce.mgp"),
      xaxs = "i", yaxs = "i", cex=par("cex"),
      adorn,
      \dots)
}
\arguments{
  \item{x,y}{locations of grid lines along which values of \code{z} are
    measured.  The values must be finite, non-missing, and in increasing
    order.}
  \item{z}{matrix containing data to be indicated on the plot.
    \code{dim(z)[1]} must equal \code{length(x)}, and the same for y.}
  \item{xlim,ylim,zlim}{limits on x axis, y axis, and the image values.}
  \item{flip.y}{boolean, with \code{TRUE} indicating that the image
    should be flipped top to bottom (as to produce a profile image
    for a downward-looking acoustic-doppler profile).}
  \item{xlab,ylab,zlab}{names for x axis, y axis, and the image values.}
  \item{breaks}{the z values for breaks in the colour scheme.}
  \item{col}{colours corresponding to the breaks.  The number of colours
    must equal the number of breaks plus 1.}
  \item{do.layout}{boolean, \code{TRUE} if \code{imagep} will be using
    the \code{\link{layout}} command to set up space for the image and
    the palette.  If set to \code{FALSE}, the user must set the layout
    first.  (The second scheme is useful for plots with several image
    panels, since \code{\link{layout}} cannot be nested.)}
  \item{draw.contours}{boolean, \code{TRUE} to get contours on the
    image, and palette, at the colour breaks.  Images with a great deal
    of high-wavenumber variation look poor with contours.}
  \item{draw.time.range}{boolean, only used if the \code{x} axis is a
    time.  If \code{TRUE}, then an indication of the time range of the
    data (not the axis) is indicated at the top-left margin of the
    graph.  This is useful because the labels on time axes only indicate
    hours if the range is less than a day, etc.}
  \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
    also for \code{par(mar)}, computed from this.  The default is
    tighter than the R default, in order to use more space for the
    data and less for the axes.}
  \item{xaxs}{character indicating whether image should extend to edge
    of x axis (with value \code{"i"}) or not; see
    \code{\link[graphics]{par}}("xaxs").}
  \item{yaxs}{as \code{xaxs} but for y axis.}
  \item{cex}{size of labels on axes and palette; see \code{\link[graphics]{par}}("cex").}
  \item{adorn}{optional expression to be performed immediately after
    drawing the data panel.}     
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Creates an image with a colour palette to the right.  The
  effect is similar to \code{\link{filled.contour}} except that with
  \code{imagep} it is possible to set the \code{\link{layout}} outside
  the function, which enables the creation of plots with many
  image-palette panels.  Note that the contour lines may not coincide
  with the colour transitions, in the case of coarse images.}

\value{None.}

\seealso{
  This is used by \code{\link{plot.adcp}}.
}
\examples{
library(oce)
h <- seq(0, 50, length.out=200)
drho <- seq(1, 3, length.out=200)
speed <- sqrt(9.8*outer(drho/1024, h, "*"))
imagep(h, drho, speed, xlab="Equivalent depth [m]",
  ylab=expression(paste(Delta*rho, " [kg/m^3]")),
  zlab="Internal-wave speed [m/s]")
}
\author{Dan Kelley}
\keyword{misc}
