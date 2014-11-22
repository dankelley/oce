\name{curl}

\alias{curl}

\title{Curl of 2D vector field}

\description{Calculate the z component of the curl of an x-y vector field.}

\usage{curl(mx, my, x, y, geographical=TRUE)}

\arguments{
    \item{mx}{matrix containing the 'x' component of a vector field}
    \item{my}{matrix containing the 'y' component of a vector field}
    \item{x}{x values for the matrices, a vector of length equal to
        \code{\link{nrow}(mx)}}
    \item{y}{y values for the matrices, a vector of length equal to
        \code{\link{ncol}(mx)}}
    \item{geographical}{logical value indicating whether \code{x} and \code{y}
        are longitude and latitude, in which case spherical trigonometry is
        used.}
}

\details{In the interior of the domain, a centred difference formula is used,
    with a 5-point stencil.  Then, the edges are filled in with
    nearest-neighbour values. Finally, the corners are filled in with the
    adjacent value along a diagonal.  If \code{geographical=TRUE}, then
    \code{x} and \code{y} are taken to be longitude and latitude in degrees,
    and the earth shape is approximated as a sphere with radius 6371km.}
    
\value{A list containing \code{x} and \code{y}, identical with the input
    values, and \code{curl}, the computed value of the vector curl.}

\examples{
library(oce)
## Construct field with curl=1.
x <- 1:4
y <- 1:10
mx <- outer(x, y, function(x,y) y/2)
my <- outer(x, y, function(x,y) -x/2)
curl(mx, my, x, y, TRUE)
## Construct something similar to mid-latitude wind-stress
lon <- seq(0, 30, 5)
lat <- seq(20, 40, 5)
taux <- outer(lon, lat, function(lon,lat) 0.1 * (lat-20)/20)
tauy <- outer(lon, lat, function(lon,lat) 0*lon)
curl(taux, tauy, lon, lat, TRUE)
}

\author{Dan Kelley and Chantelle Layton.}

\keyword{misc}

