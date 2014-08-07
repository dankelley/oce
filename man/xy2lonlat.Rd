\name{xy2lonlat}

\alias{xy2lonlat}

\title{Convert from x-y to longitude and latitude}

\description{Convert from x-y coordinates to longitude and latitude}

\usage{xy2lonlat(x, y)}

\arguments{

  \item{x,y}{coordinates of points in the projected space.}
}

\details{A projection must already have been set up, by a call to
    \code{\link{mapPlot}} or \code{\link{lonlat2xy}}.
  
    The values of \code{x} and \code{y} have a clear meaning for
    \code{proj4}-style projections.  This is not true for \code{mapproj}-style
    projections, since \code{\link{mapproject}} shifts the \code{x} component
    of its return value to have mean value equal to 0, up to a calculation
    error. 

    The accuracy of \code{xy2lonlat} tends to be higher for \code{proj4}-style
    projections than for \code{mapproj}-style projections.  This is because the
    \code{proj4} package provides inverse functions, while \code{mapproj} does
    not.  In the latter case, \code{xy2lonlat} attempts to invert the
    projection by a minimization procedure using \code{\link{optim}}, and this
    method can be quite inaccurate, depending on the projection and the
    location in question.}

\value{A list containing \code{longitude} and \code{latitude}.}

\examples{
library(oce)
## Cape Split, in the Minas Basin of the Bay of Fundy
cs <- list(longitude=-64.49657,latitude=45.33462)
xy <- lonlat2xy(cs, projection="+proj=merc")
xy2lonlat(xy)
xy <- lonlat2xy(cs, projection="mercator")
xy2lonlat(xy)
}

\seealso{\code{\link{lonlat2xy}} does the inverse operation.  For general
    projections and their inverses, use \code{\link{lonlat2xy}} and
    \code{\link{xy2lonlat}}.}

\references{\url{http://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system},
    downloaded May 31, 2014.}

\author{Dan Kelley}

\keyword{misc}

