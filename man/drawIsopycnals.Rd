% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ctd.R
\name{drawIsopycnals}
\alias{drawIsopycnals}
\title{Add Isopycnal Curves to a TS Plot}
\usage{
drawIsopycnals(
  nlevels = 6,
  levels,
  rotate = TRUE,
  rho1000 = FALSE,
  digits = 2,
  eos = getOption("oceEOS", default = "gsw"),
  longitude = NULL,
  latitude = NULL,
  trimIsopycnals = TRUE,
  gridIsopycnals = c(50, 50),
  cex = 0.75 * par("cex"),
  col = "darkgray",
  lwd = par("lwd"),
  lty = par("lty"),
  debug = getOption("oceDebug")
)
}
\arguments{
\item{nlevels}{suggested number of density levels (i.e. isopycnal curves);
ignored if \code{levels} is supplied.  If this is set to 0, no isopycnal
are drawn (see also \code{levels}, next).}

\item{levels}{optional density levels to draw.  If this is \code{NULL}, then
no isopycnals are drawn.}

\item{rotate}{boolean, set to \code{TRUE} to write all density labels
horizontally.}

\item{rho1000}{boolean, set to \code{TRUE} to write isopycnal labels as e.g.
1024 instead of 24.}

\item{digits}{minimum number of decimal digits to use in label (supplied to
\code{\link[=round]{round()}}). If the density range is very small, \code{\link[=drawIsopycnals]{drawIsopycnals()}}
will increase value of \code{digits}, to try to make labels be distinct.}

\item{eos}{equation of state to be used, either \code{"unesco"} or
\code{"gsw"}.  If it is \code{"gsw"} then \code{latitude} and \code{longitude} must
be supplied, since these are needed to computer density in that
formulation.}

\item{longitude, latitude}{numerical values giving the location
to be used in density calculations, if \code{eos} is \code{"gsw"}.}

\item{trimIsopycnals}{logical value (\code{TRUE} by default) that
indicates whether to trim isopycnal curves (if drawn)
to the region of temperature-salinity space for which density
computations are considered to be valid in the context of the
chosen \code{eos}; see the \dQuote{Details} of the documentation
for \code{\link[=plotTS]{plotTS()}}.}

\item{gridIsopycnals}{a parameter that controls how the isopycnals
are computed.  This may be NULL, or an integer vector of length 2.
\emph{Case 1:} if \code{gridIsopycnals} is NULL, then
the isopycnals are drawn by tracing density
isopleths in salinity-temperature space. This method was
used as the default prior to version 1.7-11, but it was
found to yield staircase-like isopycnal curves for highly
zoomed-in plots (e.g. with millidegree temperature ranges).
\emph{Case 2 (the new default):} If \code{gridIsopycnals} is a two-element integer
vector, then a grid of density is constructed, with \code{gridIsopycnals[1]}
salinity levels and \code{gridIsopycnals[2]} temperature levels, and
then \code{\link[=contourLines]{contourLines()}} is used to trace the isopycnals. The default
value of \code{gridIsopycnals} yields a grid of millimeter-scale spacing
for a typical plot.}

\item{cex}{size for labels.}

\item{col}{color for lines and labels.}

\item{lwd}{line width for isopycnal curves}

\item{lty}{line type for isopycnal curves}

\item{debug}{an integer specifying whether debugging information is
to be printed during the processing. This is a general parameter that
is used by many \code{oce} functions. Generally, setting \code{debug=0}
turns off the printing, while higher values suggest that more information
be printed. If one function calls another, it usually reduces the value of
\code{debug} first, so that a user can often obtain deeper debugging
by specifying higher \code{debug} values.}
}
\value{
None.
}
\description{
Adds isopycnal lines to an existing temperature-salinity plot.  This is
called by \code{\link[=plotTS]{plotTS()}}, and may be called by the user also, e.g. if
an image plot is used to show TS data density.
}
\details{
The default method of drawing isopycnals was changed in February of 2023,
so that even plots that are zoomed in to have millidegree temperature ranges
will have smooth curves.  See the discussion of \code{gridIsopycnals} for
details.
}
\references{
\itemize{
\item Fofonoff, N. P., and R. C. Millard.
"Algorithms for Computation of Fundamental Properties of Seawater."
UNESCO Technical Papers in Marine Research. SCOR working group on Evaluation of CTD data;
UNESCO/ICES/SCOR/IAPSO Joint Panel on Oceanographic Tables and Standards, 1983.
\verb{https://unesdoc.unesco.org/ark:/48223/pf0000059832}.
\item McDougall, Trevor J., David R. Jackett, Daniel G. Wright, and Rainer Feistel.
"Accurate and Computationally Efficient Algorithms for Potential Temperature and Density of Seawater."
Journal of Atmospheric and Oceanic Technology 20, no. 5 (May 1, 2003): 730-41.
\code{https://journals.ametsoc.org/jtech/article/20/5/730/2543/Accurate-and-Computationally-Efficient-Algorithms}.
}
}
\seealso{
\code{\link[=plotTS]{plotTS()}}, which calls this.
}
\author{
Dan Kelley
}
