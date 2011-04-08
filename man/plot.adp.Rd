\name{plot.adp}

\alias{plot.adp}

\title{Plot ADP data}

\description{Plot ADP data.}

\usage{\method{plot}{adp}(x, which=1:dim(x$data$ma$v)[3],
     col,
     zlim,
     titles,
     lwd=par('lwd'),
     type='l',
     ytype=c("profile", "distance"),
     adorn=NULL,
     draw.time.range=getOption("oce.draw.time.range"),
     use.smoothScatter,
     mgp=getOption("oce.mgp"),
     mar=c(mgp[1]+1.5,mgp[1]+1.5,1.5,1.5),
     margins.as.image=FALSE,
     cex=par("cex"), cex.axis=par("cex.axis"), cex.main=par("cex.main"),
     xlim, ylim, 
     control,
     use.layout=FALSE,
     main="",
     debug=getOption("oce.debug"),
     \dots)}

\arguments{
  \item{x}{an \code{adp} object, e.g. as read by \code{\link{read.adp}}.}
  \item{which}{list of desired plot types.  These are graphed in
    panels running down from the top of the page.  See
    \dQuote{Details} for the meanings of various values of
    \code{which}.}
  \item{col}{optional indication of colour(s) to use.  If not provided,
    the default for images is \code{oce.colors.palette(128,1)}, and for
    lines and points is black.}
  \item{zlim}{a range to be used as the \code{zlim} parameter to the
    \code{\link{imagep}} call that is used to create the image.  If
    omitted, \code{zlim} is set for each panel individually, to
    encompass the data of the panel and to be centred around zero.
    If provided as a two-element vector, then that is used for each panel.
    If provided as a two-column matrix, then each panel of the graph uses
    the corresponding row of the matrix; for example, setting
    \code{zlim=rbind(c(-1,1),c(-1,1),c(-.1,.1))} might make sense for
    \code{which=1:3}, so that the two horizontal velocities have one
    scale, and the smaller vertical velocity has another.}
  \item{titles}{optional vector of character strings to be used as
    labels for the plot panels.  For images, these strings will be
    placed in the right hand side of the top margin.  For timeseries,
    these strings are ignored.
    If this is provided, its length must equal that of \code{which}.}
  \item{lwd}{if the plot is of a time-series or scattergraph format with lines,
      this is used in the usual way; otherwise, e.g. for image formats, this is ignored.}
  \item{type}{if the plot is of a time-series or scattergraph format, this is
      used in the usual way, e.g. \code{"l"} for lines, etc.; otherwise, as for
      image formats, this is ignored.}
  \item{ytype}{character string controlling the type of the y axis for
    images (ignored for time series).  If \code{"distance"}, then the y
    axis will be distance from the sensor head, with smaller distances
    nearer the bottom of the graph.  If \code{"profile"}, then this will
    still be true for upward-looking instruments, but the y axis will be
    flipped for downward-looking instruments, so that in either case,
    the top of the graph will represent the sample nearest the sea
    surface.}
  \item{adorn}{list of expressions to be executed for the panels in
    turn, e.g. to adorn the plots.  If the number matches the number of
    panels, then the strings are applied to the appropriate panels, 
    as they are drawn from top-left to bottom-right.   If only a single
    expression is provided, it is used for all panels.  (See
    \dQuote{Examples}.)}
  \item{draw.time.range}{boolean that applies to panels with time as the
    horizontal axis, indicating whether to draw the time range in 
    the top-left margin of the plot.}
  \item{use.smoothScatter}{boolean that indicates whether to use
      \code{\link{smoothScatter}} in various plots, such as \code{which="uv"}.
      If not provided a default is used, with \code{\link{smoothScatter}} being
      used if there are more than 2000 points to plot.},
  \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
    also for \code{par(mar)}, computed from this.  The default is
    tighter than the R default, in order to use more space for the data
    and less for the axes.}
  \item{mar}{value to be used with \code{\link{par}("mar")}.}
  \item{margins.as.image}{boolean, \code{TRUE} to put a wide margin to the
    right of time-series plots, even if there are no images in the \code{which}
    list.  (The margin is made wide if there are some images in the sequence.)}
  \item{cex}{size of labels on axes; see \code{\link[graphics]{par}}("cex").}
  \item{cex.axis}{see \code{\link[graphics]{par}}("cex.axis").}
  \item{cex.main}{see \code{\link[graphics]{par}}("cex.main").}
  \item{xlim}{optional 2-element list for \code{xlim}, or 2-column matrix, in which case
    the rows are used, in order, for the panels of the graph.}
  \item{ylim}{optional 2-element list for \code{ylim}, or 2-column matrix, in which case
    the rows are used, in order, for the panels of the graph.}
  \item{control}{optional list of parameters that may be used for
    different plot types.  Possibilities are \code{draw.bottom} (which
    means to draw the bottom, for downward-pointing units that track the
    bottom) and \code{bin} (for selecting a bin on which to act, as
    explained in \dQuote{Details}).}
  \item{use.layout}{set to \code{FALSE} to prevent using
    \code{\link{layout}} to set up the plot.  This is needed if the call
    is to be part of a sequence set up by e.g. \code{par(mfrow)}.}
  \item{main}{main title for plot, used just on the top panel, if there are several panels.}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
    moderate amount of debugging information, or to 2 to get more.} 
  \item{\dots}{optional arguments passed to plotting functions,
    e.g. supplying \code{despike=TRUE} will cause time-series panels to
    be de-spiked with \code{\link{despike}}.}
}

\details{Creates a multi-panel summary plot of data measured by an ADP
  cast. The panels are controlled by the \code{which} argument.

  \itemize{ 

    \item \code{which=1:4} (or \code{which="u1"} to \code{"u4"}) yield a
      distance-time image plot of a velocity component.  If \code{x} is
      in \code{beam} coordinates (signalled by
      \code{x$metadata$oce.coordinate=="beam"}), this will be the beam
      velocity, labelled \code{b[1]} etc.  If \code{x} is in xyz
      coordinates (sometimes called frame coordinates, or ship
      coordinates), it will be the velocity component to the right of
      the frame or ship (labelled \code{u} etc).  Finally, if \code{x}
      is in \code{"enu"} coordinates, the image will show the the
      eastward component (labelled \code{east}).  If \code{x} is in
      \code{"other"} coordinates, it will be component corresponding to
      east, after rotation (labelled \code{u\'}).  Note that the
      coordinate is set by \code{\link{read.adp}}, or by
      \code{\link{beam.to.xyz.adp}}, \code{\link{xyz.to.enu.adp}}, or
      \code{\link{enu.to.other.adp}}.
    
    \item \code{which=5:8} (or \code{which="a1"} to \code{"a4"}) yield
      distance-time images of backscatter intensity of the respective beams.

    \item \code{which=9:12} (or \code{which="q1"} to \code{"q4"}) yield
      distance-time images of signal quality for the respective beams.

    \item \code{which=13} (or \code{which="salinity"}) yields a
      time-series plot of salinity.

    \item \code{which=14} (or \code{which="temperature"}) yields a
      time-series plot of temperature.

    \item \code{which=15} (or \code{which="pressure"}) yields a
      time-series plot of pressure.

    \item \code{which=16} (or \code{which="heading"}) yields a
      time-series plot of instrument heading.

    \item \code{which=17} (or \code{which="pitch"}) yields a time-series
      plot of instrument pitch.

    \item \code{which=18} (or \code{which="roll"}) yields a time-series
      plot of instrument roll.

    \item \code{which=19} yields a time-series plot of distance-averaged
      velocity for beam 1, rightward velocity, eastward velocity, or
      rotated-eastward velocity, depending on the coordinate system.

    \item \code{which=20} yields a time-series of distance-averaged
      velocity for beam 2, foreward velocity, northward velocity, or
      rotated-northward velocity, depending on the coordinate system.

    \item \code{which=21} yields a time-series of distance-averaged
      velocity for beam 3, up-frame velocity, upward velocity, or
      rotated-upward velocity, depending on the coordinate system.

    \item \code{which=22} yields a time-series of distance-averaged
      velocity for beam 4, for \code{beam} coordinates, or velocity
      estimate, for other coordinates.  (This is ignored for 3-beam
      data.)

    \item \code{which=23} yields a progressive-vector diagram in the
      horizontal plane, plotted with \code{asp=1}.  Normally, the
      depth-averaged velocity components are used, but if the
      \code{control} list contains an item named \code{bin}, then the
      depth bin will be used (with an error resulting if the bin is out
      of range).

    \item \code{which=24} yields a time-averaged profile of the first
      component of velocity (see \code{which=19} for the meaning of the
      component, in various coordinate systems).

    \item \code{which=25} as for 24, but the second component.

    \item \code{which=26} as for 24, but the third component.

    \item \code{which=27} as for 24, but the fourth component (if that
      makes sense, for the given instrument).

    \item \code{which=28} or \code{"uv"} yields velocity plot in the
      horizontal plane, i.e. u[2] versus u[1].  If the number of data
      points is small, a scattergraph is used, but if it is large,
      \code{\link{smoothScatter}} is used.

    \item \code{which=29} or \code{"uv+ellipse"} as the \code{"uv"}
      case, but with an added indication of the tidal ellipse,
      calculated from the eigen vectors of the covariance matrix.

    \item \code{which=30} or \code{"uv+ellipse+arrow"} as the
      \code{"uv+ellipse"} case, but with an added arrow indicating the
      mean current.

    \item \code{which=40} or \code{"bottom.range"} for average bottom range
      from all beams of the instrument.

    \item \code{which=41} to \code{44} (or \code{"bottom.range1"} to
      \code{"bottom.range1"}) for bottom range from beams 1 to 4.

    \item \code{which=50} or \code{"bottom.u"} for average bottom velocity
      from all beams of the instrument.

    \item \code{which=51} to \code{54} (or \code{"bottom.u1"} to
      \code{"bottom.u4"}) for bottom velocity from beams 1 to 4.

    \item \code{which=55} (or \code{"heaving"} for time-integrated,
    depth-averaged, vertical velocity (i.e. heaving)

    }
    In addition to the above, there are some groupings defined:
    \itemize{
        \item \code{which="velocity"} equivalent to \code{which=1:3} (velocity components)
        \item \code{which="amplitude"} equivalent to \code{which=5:7} (backscatter intensity components)
        \item \code{which="quality"} equivalent to \code{which=9:11} (quality components)
        \item \code{which="hydrography"} equivalent to \code{which=14:15} (temperature and pressure)
        \item \code{which="angles"} equivalent to \code{which=16:18} (heading, pitch and roll)
    }

    The color scheme for image plots (\code{which} in 1:12) is provided
    by the \code{col} argument, which is passed to \code{\link{image}}
    to do the actual plotting.  See \dQuote{Examples} for some
    comparisons.

    A common quick-look plot to assess mooring movement is to use
    \code{which=15:18} (pressure being included to signal the tide, and
    tidal currents may dislodge a mooring or cause it to settle).

    By default, \code{plot.adp} uses a \code{zlim} value for the
    \code{\link{image}} that is constructed to contain all the data, but
    to be symmetric about zero.  This is done on a per-panel basis, and
    the scale is plotted at the top-right corner, along with the name of
    the variable being plotted. You may also supply \code{zlim} as one
    of the \dots arguments, but be aware that a reasonable limit on
    horizontal velocity components is unlikely to be of much use for the
    vertical component.

    A good first step in the analysis of measurements made from a moored
    device (stored in \code{d}, say) is to do \code{plot(d,
    which=14:18)}.  This shows time series of water properties and
    sensor orientation, which is helpful in deciding which data to trim
    at the start and end of the deployment, because they were measured
    on the dock or on the ship as it travelled to the mooring site.
}

\seealso{The data are normally read with \code{\link{read.oce}}, which
  is usually able to detect the type of instrument.  If that fails, the
  user should try \code{\link{read.adp.rdi}} (for RDI files) or
  \code{\link{read.adp.nortek}} (for NorTek files).  These files may be
  in beam coordinates, in xyz coordinates, or in enu coordinates.  It is
  common in a scientific setting to use beam coordinates, and so this
  package offers functions \code{\link{beam.to.xyz.adp}} and
  \code{\link{xyz.to.enu.adp}} to take the intermediate steps, and
  \code{\link{enu.to.other.adp}} to go further, e.g. to rotate into a
  coordinate system aligned with the shoreline or with an isobath.

  The beam stength may be corrected for R-squared spreading with
  \code{\link{beam.attenuate.adp}}.

  Summaries of ADP data are provided by \code{\link{summary.adp}}.

  ADP data may be plotted with \code{\link{plot.adp}}.
}

\examples{
library(oce)
data(adp)
plot(adp, which=1:3)
# Illustrate adornment
plot(adp, which=1:2,
     adorn=expression({
         abline(v=as.POSIXct('2008-06-26 06:00:00',tz='UTC'),col='green',lwd=3)
         lines(x$data$ts$time, x$data$ts$pressure, lwd=3, col='blue')
     }))
}

\author{Dan Kelley}
\keyword{misc}
