\name{approx3d}

\alias{approx3d}

\title{Trilinear interpolation in a 3D array}

\description{Interpolate within a 3D array, using the trilinear approximation.}

\usage{approx3d(x, y, z, f, xout, yout, zout)}

\arguments{

    \item{x}{vector of x values for grid (must be equi-spaced)}

    \item{y}{vector of y values for grid (must be equi-spaced)}
    
    \item{z}{vector of z values for grid (must be equi-spaced)}

    \item{f}{matrix of rank 3, with the gridd values mapping to the \code{x}
        values (first index of \code{f}), etc.}

    \item{xout}{vector of x values for output.}

    \item{yout}{vector of y values for output (length must match that of
        \code{xout}).}

    \item{zout}{vector of z values for output (length must match that of
        \code{xout}).}

}


\details{For values of \code{xout}, \code{yout} and \code{zout} that are inside
    the region specified by \code{x}, \code{y} and \code{z}, use trilinear
    interpolation within the grid cube containing the point.  For values
    outside the cube, return \code{NA}.}

    
\value{A vector of interpolated values, with length matching that of
    \code{xout}.  Any points that are outside the grid yield \code{NA} values.}

    
\examples{
## set up a grid
library(oce)
n <- 5
x <- seq(0, 1, length.out=n)
y <- seq(0, 1, length.out=n)
z <- seq(0, 1, length.out=n)
f <- array(1:n^3, dim=c(length(x), length(y), length(z)))
## interpolate along a diagonal line
m <- 100
xout <- seq(0, 1, length.out=m)
yout <- seq(0, 1, length.out=m)
zout <- seq(0, 1, length.out=m)
approx <- approx3d(x, y, z, f, xout, yout, zout)
## graph the results
plot(xout, approx, type='l')
points(xout[1], f[1,1,1])
points(xout[m], f[n,n,n])
}

\author{Dan Kelley}

\keyword{misc}

