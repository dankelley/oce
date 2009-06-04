\name{plot.adcp}

\alias{plot.adcp}

\title{Plot ADCP data}

\description{Plot ADCP data.}

\usage{\method{plot}{adcp}(x, which=1:x$metadata$number.of.beams, 
     col=oce.colors.palette(128,1), zlim, 
     titles,
     ytype=c("profile", "distance"),
     adorn=NULL,
     draw.time.range=getOption("oce.draw.time.range"),
     mgp=getOption("oce.mgp"),
     mar=c(mgp[1],mgp[1]+1,1,1),
     \dots)}

\arguments{
  \item{x}{an \code{adcp} object, e.g. as read by \code{\link{read.adcp}}.}
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
    (Centering on zero is convenient for color palettes that place a
    neutral color there; see \dQuote{Details}.)}
  \item{titles}{optional vector of character strings to be used as
    labels for the plot panels.  For images, these strings will be
    placed in the right hand side of the top margin.  For timeseries,
    these strings are ignored.
    If this is provided, its length must equal that of \code{which}.}
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
  \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
    also for \code{par(mar)}, computed from this.  The default is
    tighter than the R default, in order to use more space for the data
    and less for the axes.}
  \item{mar}{value to be used with \code{\link{par}("mar")}.}
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Creates a multi-panel summary plot of data measured by an ADCP
  cast. The panels are controlled by the \code{which} argument.

  \itemize{ 
    
    \item For data in \code{beam} coordinates, \code{which}=1 gives an
    image plot of beam 1 as a function of time and distance from sensor;
    \code{which} values of 2, 3 (and 4, if applicable) yield similar
    plots for the other beams.

    \item For \code{frame} data, as created by
    \code{\link{adcp.beam2frame}}, \code{which}=1, 2, and 3 yield u, v,
    and w components of velocity, and, for the case of 4-beam
    instruments such as \code{rdi}, \code{which}=4 yields a velocity
    error estimate computed from all beams together.

    \item For \code{earth} data, as created by
    \code{\link{adcp.frame2earth}}, \code{which}=1, 2, and 3 yield
    \code{east}-, \code{north}-, and \code{up}-ward components of
    velocity, and \code{which}=4 yields a velocity error estimate
    computed from all beams together.

    \item For all types of data: \code{which} values of 5 through 8
    yield image of backscatter intensity of beams 1 through 4,
    respectively, and \code{which} values of 9 through 11 (or 12) yield
    measures of the signal quality for the individual beams.

    \item Time-series of scalar quantities can also be plotted:
    \code{which}=13 yields salinity, 14 yields temperature, 15 yields
    pressure at sensor head, 16 yields instrument heading, 17 yields
    instrument pitch, and 18 yields instrument roll.  A common
    quick-look plot to assess mooring movement is to use
    \code{which=15:18} (pressure being included to signal the tide, and
    tidal currents may dislodge a mooring or cause it to settle).

  }

  The color scheme for image plots (\code{which} in 1:12) is provided
  by the \code{col} argument, which is passed to \code{\link{image}} to
  do the actual plotting.  See \dQuote{Examples} for some comparisons.

  By default, \code{plot.adcp} uses a \code{zlim} value for the
  \code{\link{image}} that is constructed to contain all the data, but
  to be symmetric about zero.  This is done on a per-panel basis, and
  the scale is plotted at the top-right corner, along with the name of
  the variable being plotted. You may also supply \code{zlim} as one of
  the \dots arguments, but be aware that a reasonable limit on
  horizontal velocity components is unlikely to be of much use for the
  vertical component.

  A good first step in the analysis of measurements made from a moored
  device (stored in \code{d}, say) is to do \code{plot(d, which=14:18)}.
  This shows time series of water properties and sensor orientation,
  which is helpful in deciding which data to trim at the start and end
  of the deployment, because they were measured on the dock or on the
  ship as it travelled to the mooring site.
}

\seealso{The data are normally read with \code{\link{read.oce}}, which
is usually able to detect the type of instrument.  If that fails, the user
should try \code{\link{read.adcp.rdi}} (for RDI files) 
or \code{\link{read.adcp.nortek}} 
(for NorTek files).  These files may be in beam coordinates, 
in frame coordinates, or in earth coordinates.  It is common in a scientific
setting to use beam coordinates, and so this package offers functions
\code{\link{adcp.beam2frame}} and 
\code{\link{adcp.frame2earth}} to take the intermediate steps, and
\code{\link{adcp.earth2other}} to go further, e.g. to rotate into
a coordinate system aligned with the shoreline or with an isobath.

The beam stength may be corrected for R-squared spreading with
\code{\link{adcp.beam.attenuate}}.  

Summaries of ADCP data are provided by \code{\link{summary.adcp}}.

ADCP data may be plotted with \code{\link{plot.adcp}}.
}

\examples{
library(oce)
data(adcp)
e <- adcp.frame2earth(adcp.beam2frame(adcp))
plot(e, which=1:3)
# Illustrate adornment
plot(e, which=1:2,
     adorn=expression({
         abline(v=as.POSIXct('2008-06-26 06:00:00',tz='UTC'),col='green',lwd=3)
         lines(x$data$ts$time, x$data$ts$pressure, lwd=3, col='blue')
     }))
}

\author{Dan Kelley}
\keyword{misc}



