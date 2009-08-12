\name{oce.plot.ts}
\alias{oce.plot.ts}

\title{Plot a time-series, obeying the timezone}

\description{Plot a time-series, obeying the timezone and possibly drawing the range in the top-left margin}

\usage{oce.plot.ts(x, y, draw.time.range=TRUE, xaxs="i", grid=TRUE, adorn=NULL, fill=FALSE, \dots)}

\arguments{
\item{x}{the times of observations.}
\item{y}{the observations.}
\item{draw.time.range}{a boolean, set to \code{TRUE} to indicate the range of times 
  in the top-left margin.}
\item{xaxs}{character indicating whether image should extend to edge
  of x axis (with value \code{"i"}) or not; see
  \code{\link[graphics]{par}}("xaxs").}
\item{grid}{boolean, set to \code{TRUE} to get a grid on the plot.  (Note that
  \code{\link{grid}} does work correctly because it does not account
  properly for tic locations on the time axis.}
\item{adorn}{optional \code{\link{expression}} to be performed immediately after
  drawing the data panel.}     
\item{fill}{boolean, set \code{TRUE} to fill the curve to zero (which it
  does incorrectly if there are missing values in \code{y}).}
\item{\dots}{graphical parameters passed down to \code{\link{plot}}.}
}

\details{Depending on the version of R, the standard \code{\link{plot}} and
\code{\link{plot.ts}} routines will not obey the time zone of the
data.  This routine gets around that problem.  It can also plot the
time range in the top-left margin, if desired; this string includes
the timezone, to remove any possible confusion.

For flexibility, this does not set to line-type plots; use \code{type='l'} to get these.
}

\examples{
library(oce)
t.start <- as.POSIXct("2008-01-01", tz="UTC")
t <- seq(t.start, length.out=48, by="30 min")
y <- sin(as.numeric(t - t.start) * 2 * pi / (12 * 3600))
oce.plot.ts(t, y, type='l', xaxs='i')
}

\references{
Color Brewer.
http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer.html

Light, A., and P. J. Bartlein, 2004.
The End of the Rainbow? Color Schemes for Improved Data Graphics.
\emph{Eos Trans. AGU}, 85(40), doi:10.1029/2004EO400002.

Martin Jakobsson, Ron Macnab, and Members of the Editorial Board, IBCAO.
Selective comparisons of GEBCO (1979) and IBCAO (2000) maps.
\url{http://www.ngdc.noaa.gov/mgg/bathymetry/arctic/ibcao_gebco_comp.html}

Stephenson, David B., 2005.
Comment on ``Color schemes for improved data graphics,''
by A. Light and P. J. Bartlein.
\emph{Eos Trans. AGU}, 86(20).


}

\author{Dan Kelley}

\keyword{misc}
