\name{curl}

\alias{curl}

\title{Curl of 2D vector field}

\description{Calculate the z component of the curl of an x-y vector field.}

\usage{curl(mx, my, x, y, geographical=FALSE, method=1)}

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
    \item{method}{A number indicating the method to be used to calculate the
        first-difference approximations to the derivatives.  See
        \dQuote{Details}.}
}

\details{The computed component of the curl is defined by \eqn{\partial
        m_y/\partial x - \partial m_x/\partial y}{d(m_y)/dx - d(m_x)/dy} and
    the estimate is made using first-difference approximations to the
    derivatives.  Two methods are provided, selected by the value of
    \code{method}.

	\itemize{

          \item{1}{For \code{method=1}, a centred-difference, 5-point stencil
              is used in the interior of the domain.  Then, the edges are
              filled in with nearest-neighbour values. Finally, the corners are
              filled in with the adjacent value along a diagonal.  If
              \code{geographical=TRUE}, then \code{x} and \code{y} are taken to
              be longitude and latitude in degrees, and the earth shape is
              approximated as a sphere with radius 6371km.  The resultant
              \code{x} and \code{y} are identical to the provided values, and
              the resultant \code{curl} is a matrix with dimension identical to
              that of \code{mx}.}

          \item{2}{FIXME.CL: write description of CL method here.  Try to mimic
              the typographical and language style used for method=1, e.g. end
              with a note on the lengths of x and y, and dimension of curl.}

        }
}

\value{A list containing vectors \code{x} and \code{y}, along with matrix
    \code{curl}.  See \dQuote{Details} for the lengths and dimensions, for
    various values of \code{method}.}

\examples{
library(oce)
## 1. constant curl
x <- 1:4
y <- 1:10
mx <- outer(x, y, function(x,y) y/2)
my <- outer(x, y, function(x,y) -x/2)
C <- curl(mx, my, x, y, FALSE)

## 2. crude model of mid-latitude wind-stress
lon <- seq(0, 100, 50)
lat <- seq(20, 40, 1)
taux <- outer(lon, lat, function(lon,lat) 0.1 * sin((lat-20)/20*pi/2))
tauy <- outer(lon, lat, function(lon,lat) 0*lon)
C <- curl(taux, tauy, lon, lat, TRUE)
contour(C$x, C$y, C$curl)
}

\author{Dan Kelley and Chantelle Layton.}

\keyword{misc}

