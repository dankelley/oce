\name{plot.profile}

\alias{plot.profile}

\title{Plot a CTD profile of various quantities}

\description{Plot a CTD profile, in any of several common formats.}

\usage{
plot.profile(x,
type="S", 
col.S="darkgreen",
col.t="red",
col.rho="blue",
col.N2="brown",
grid=FALSE, 
col.grid="lightgray",
Slim, Tlim, densitylim, N2lim, plim,
\dots)
}

\arguments{
  \item{x}{A \code{cdt} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{type}{Type of profile to plot, from the list below.
      \item{\code{S}}{Profile of salinity.}
      \item{\code{T}}{Profile of \emph{in-situ} temperature.}
      \item{\code{density}}{Profile of density (expressed as \eqn{\sigma_\theta}{sigma_theta}).}
      \item{\code{S+T}}{Profile of salinity and temperature within a single axis frame.}
      \item{\code{N2}}{Profile of square of buoyancy frequency
	\eqn{N^2}{N^2}, calculated with \code{\link{sw.N2}} with an
	optional argument setting of \code{df=length(x$p)/4} to do 
	some smoothing.}
      \item{\code{density+N2}}{Profile of sigma-theta and the square of buoyancy
	frequency within a single axis frame.}
  }
  \item{col.S}{Color for salinity profile.}
  \item{col.t}{Color for temperature.}
  \item{col.rho}{Color for density.}
  \item{col.N2}{Color for square of buoyancy frequency.}
  \item{grid}{Set \code{TRUE} to get a grid.}
  \item{col.grid}{Grid colour.}
  \item{Slim}{Optional limit for S axis}
  \item{Tlim}{Optional limit for T axis}
  \item{densitylim}{Optional limit for density axis}
  \item{N2lim}{Optional limit for N2 axis}
  \item{plim}{Optional limit for p axis}
  \item{\dots}{Optional arguments passed to other functions.  A common example is to set \code{df}, for 
use in \link{sw.N2} calculations.} 
}

\value{None.}

\seealso{\code{\link{read.ctd}} scans ctd information from a file, and
  \code{\link{plot.TS}} plots a temperature-salinity diagram. 
}

\examples{
\dontrun{
library(oce)
data(ctd)
# ctd <- read.ctd("ctd.cnv")
summary(ctd)
plot(ctd)
plot.profile(ctd, type="T")
}
}

\author{Dan Kelley}

\keyword{misc}
