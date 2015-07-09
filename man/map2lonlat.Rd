% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{map2lonlat}

\alias{map2lonlat}

\title{Convert from x-y to longitude and latitude}

\description{Convert from x-y coordinates to longitude and latitude. This is
  normally called internally within oce; see \sQuote{Bugs}.}

\usage{map2lonlat(x, y, init=c(0,0))}

\arguments{

  \item{x}{a vector containing the x component of points in the projected space,
    or a list containing items named \code{x} and \code{y}, in which case the
    next argument is ignored.}

  \item{y}{a vector containing the y coordinate of points in the projected space
    (ignored if \code{x} is a list, as described above).}

  \item{init}{a vector containing the initial guesses for longitude and
    latitude, presently ignored.}

}

\details{A projection must already have been set up, by a call to
  \code{\link{mapPlot}} or \code{\link{lonlat2map}}. It should be noted that not
  all projections are handled well; see \sQuote{Bugs}.}

\section{Bugs}{\code{oce} uses \link[rgdal]{project} in the \CRANpkg{rgdal}
  package to handle projections. Only those projections that have inverses are
  permitted within \code{oce}, but this does not mean that \emph{all} data can
  be inverted with \code{map2lonlat}. For example, as of June 2015, the
  \CRANpkg{rgdal} version of the Winkel Tripel projection (from \code{mapPlot(,
    ..., projection="+proj=wintri", ...)}) handles "off globe" data poorly,
  either taking what seems to be an infinite time to do the calculation, or even
  crashing R. This is due to a loop in the \CRANpkg{rgdal} C code (file
  \code{PJ_aitoff.c}) failing to converge for "off globe" points. Not all setups
  have this difficulty, however, and since the problem was reported to the
  PROJ.4 team (email exchanges from April 10 to April 13, 2015), one can hope
  for a PROJ.4 solution sometime in 2015, propagating to R and thereafter
  \CRANpkg{rgdal} in due course.}
  
\value{A list containing \code{longitude} and \code{latitude}, with \code{NA}
  values indicating points that are off the globe as displayed.}

\examples{
library(oce)
## Cape Split, in the Minas Basin of the Bay of Fundy
cs <- list(longitude=-64.49657,latitude=45.33462)
xy <- lonlat2map(cs, projection="+proj=merc")
map2lonlat(xy)
}

\seealso{\code{\link{lonlat2map}} does the inverse operation.}

\author{Dan Kelley}

\keyword{misc}

