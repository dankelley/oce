\name{map2lonlat}

\alias{map2lonlat}

\title{Convert from x-y to longitude and latitude}

\description{Convert from x-y coordinates to longitude and latitude}

\usage{map2lonlat(x, y, init=c(0,0))}

\arguments{

  \item{x}{a vector containing the x component of points in the projected
      space, or a list containing items named \code{x} and \code{y}, in which
      case the next argument is ignored.}

  \item{y}{a vector containing the y coordinate of points in the projected
      space (ignored if \code{x} is a list, as described above).}

  \item{init}{a vector containing the initial guesses for longitude and
      latitude, used only for \code{mapproj}-style projections (see
      \code{\link{mapPlot}}.  If the default produces inaccurate results, try
      setting \code{init} to a point in the domain of interest (at least the
      right hemisphere).}

}

\details{A projection must already have been set up, by a call to
    \code{\link{mapPlot}} or \code{\link{lonlat2map}}.
  
    The values of \code{x} and \code{y} have a clear meaning for
    \code{rgdal}-style projections.  This is not true for \code{mapproj}-style
    projections, since \code{mapproject} in the \code{mapproj} package shifts
    the \code{x} component of its return value to have mean value equal to 0,
    up to a calculation error. 

    The accuracy of \code{map2lonlat} tends to be higher for \code{rgdal}-style
    projections than for \code{mapproj}-style projections.  This is because the
    \code{rgdal} package provides inverse functions, while \code{mapproj} does
    not, so that in the latter case the longitude and latitude are determined
    by a minimization procedure using \code{\link{optim}}.  See \dQuote{Bugs}.

}

\value{A list containing \code{longitude} and \code{latitude}, with \code{NA}
    values indicating points that are off the globe as displayed.}

\section{Bugs}{The \code{mapproj}-style case can yield \code{NA} for longitude
    and latitude, even for what would seem to be perfectly benign conditions.
    Since \code{map2lonlat} is called by many mapping functions, this can yield
    errors of various types, e.g. unaesthetic axis labelling.  In many cases
    those errors can be circumvented by switching to the equivalent
    \code{rgdal}-style projection.}

\examples{
library(oce)
## Cape Split, in the Minas Basin of the Bay of Fundy
cs <- list(longitude=-64.49657,latitude=45.33462)
xy <- lonlat2map(cs, projection="+proj=merc")
map2lonlat(xy)
}

\seealso{\code{\link{lonlat2map}} does the inverse operation.}

\references{\url{http://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system},
    downloaded May 31, 2014.}

\author{Dan Kelley wrote the \code{map2lonlat()} and the C code that
    connects with PROJ.4 code. The PROJ.4 code was written by Gerald
    Evenden, Frank Warmerdam, Howard Butler, Brent Fraser, Chris Stuber,
    Craig Bruce, Victor Osipkov, Andrea Antonello, Charles Karney,
    Karsten Engsager, and Knud Poder.}

\keyword{misc}

