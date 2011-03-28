\name{rescale}

\alias{rescale}

\title{Rescale values to lie in a given range}

\description{Rescale values to lie in a given range}

\usage{rescale(x, lim=c(0,1))}

\arguments{
    \item{x}{a numeric vector.}
    \item{lim}{two-element vector giving the range of the result.}
}

\details{This is helpful in e.g. developing a color scale for an image plot.}

\value{A new vector, which has minimum \code{lim[1]} and maximum \code{lim[2]}.}

\examples{
x <- runif(1000, min=-1, max=1)
y <- runif(1000, min=-1, max=1)
drawpalette(zlim=c(-2,2), zlab=expression(x^2+y^2), col=oce.colors.jet)
col <- oce.colors.jet(100)[rescale(x^2+y^2, c(1, 100))]
plot(x, y, col=col, cex=4, pch=20)
}

\author{Dan Kelley}

\keyword{misc}
