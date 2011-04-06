\name{rescale}

\alias{rescale}

\title{Rescale values to lie in a given range}

\description{Rescale values to lie in a given range}

\usage{rescale(x, xlow, xhigh, rlow=0, rhigh=1, clip=TRUE)}

\arguments{
    \item{x}{a numeric vector.}
    \item{xlow}{\code{x} value to correspond to \code{rlow}.  If not given, it will be
        calculated as the minimum value of \code{x}} 
    \item{xhigh}{\code{x} value to correspond to \code{rhigh}.  If not given, it will be
        calculated as the maximum value of \code{x}} 
    \item{rlow}{value of the result corresponding to \code{x} equal to \code{xlow}.}
    \item{rhigh}{value of the result corresponding to \code{x} equal to \code{xhigh}.}
    \item{clip}{logical, set to \code{TRUE} to clip the result to the range \code{rlow} to \code{rhigh}.}
}

\details{This is helpful in e.g. developing a color scale for an image plot.}

\value{A new vector, which has minimum \code{lim[1]} and maximum \code{lim[2]}.}

\examples{
x <- runif(1000, min=-1, max=1)
y <- runif(1000, min=-1, max=1)
drawpalette(zlim=c(0,2), zlab=expression(x^2+y^2), col=oce.colors.jet)
col <- oce.colors.jet(256)[rescale(x^2+y^2, rlow=1, rhigh=256)]
plot(x, y, col=col, cex=4, pch=20)
}

\author{Dan Kelley}

\keyword{misc}
