\name{plot.aquadopp}

\alias{plot.aquadopp}

\title{Plot AQUADOPP data}

\description{Plot AQUADOPP data.}

\usage{\method{plot}{aquadopp}(x, which=1:3, col=oce.colors.palette(128,1), zlim,
	 titles,
         adorn=NULL,
         draw.timerange=getOption("oce.draw.timerange"),
         mgp=getOption("oce.mgp"), \dots)}

\arguments{
  \item{x}{An \code{aquadopp} object, e.g. as read by \code{\link{read.aquadopp}}.}
  \item{which}{list of desired plot types.  These are graphed in
    panels running down from the top of the page.  See
    \dQuote{Details} for the meanings of various values of
    \code{which}.}
  \item{col}{color palette to use.  The default
  \code{\link{oce.colors.two}} palette blends from blue for negative
     values, through white at zero, to red at positive values; see
     \dQuote{Details}.}
  \item{zlim}{a range to be used as the \code{zlim} parameter to the
    \code{\link{imagep}} call that is used to create the image.  If
    omitted, \code{zlim} is set for each panel individually, to
    encompass the data of the panel and to be centred around zero.
    (Centering on zero is convenient for color palettes that place a
    neutral color there; see \dQuote{Details}.)}
  \item{titles}{optional vector of character strings to be used as
    labels for the plot panels.  For images, these strings will be
    placed in the right hand side of the top margin.  For timeseries,
    these strings are ignored.
    If this is provided, its length must equal that of \code{which}.}
  \item{adorn}{list of expressions to be executed for the panels in
    turn, e.g. to adorn the plots.  If the number matches the number of
    panels, then the strings are applied to the appropriate panels,
    as they are drawn from top-left to bottom-right.   If only a single
    expression is provided, it is used for all panels.  (See
    \dQuote{Examples}.)}
  \item{draw.timerange}{boolean that applies to panels with time as the
    horizontal axis, indicating whether to draw the time range in
    the top-left margin of the plot.}
  \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
    also for \code{par(mar)}, computed from this.  The default is
    tighter than the R default, in order to use more space for the data
    and less for the axes.}
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Creates a multi-panel summary plot of data measured by an AQUADOPP
  cast. The panels are controlled by the \code{which} argument.

  \itemize{

    \item For \code{beam} data, as created by \code{\link{read.aquadopp}} or
    \code{\link{read.oce}}, \code{which}=1 gives an image plot of beam 1
    as a function of time and distance from sensor, and \code{which}
    values of 2 and 3 yield similar plots for the other beams.

    \item \code{which}=4 is ignored, and is only present to agree with \code{\link{plot.adcp}}.

    \item For all types of data: \code{which} values of 5 through 7
    yield images of amplitude.

    \item \code{which}=8 is ignored, and is only present to agree with \code{\link{plot.adcp}}.

    \item \code{which} values of 9 through 11 yield the
    correlation for beams 1 through 3.

    \item \code{which}=8 is ignored, and is only present to agree with \code{\link{plot.adcp}}.

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

  By default, \code{plot.aquadopp} uses a \code{zlim} value for the
  \code{\link{image}} that is constructed to contain all the data, but
  to be symmetric about zero.  This is done on a per-panel basis, and
  the scale is plotted at the top-right corner, along with the name of
  the variable being plotted. You may also supply \code{zlim} as one of
  the \dots arguments, but be aware that a reasonable limit on
  horizontal velocity components is unlikely to be of much use for the
  vertical component.
}

\seealso{The data may be read with \code{\link{read.aquadopp}}
  and summarized with \code{\link{summary.aquadopp}}.
}

\references{General information on the aquadopp profiler, and other
  NorTek products, is available at \url{http://www.nortekusa.com/}.}

\examples{
\dontrun{
library(oce)
data(aquadopp)
plot(quadopp)
plot(aquadopp, which=1:2,
     adorn=expression({
         abline(v=as.POSIXct('2008-06-26 06:00:00',tz='UTC'),col='green',lwd=3)
         lines(x$data$ts$time, x$data$ts$pressure, lwd=3, col='blue')
     } ) )
}
}

\author{Dan Kelley}
\keyword{misc}
