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
x <- 1:6
drawpalette(c(1,6),col=oce.colors.jet)
plot(x, pch=20, cex=6, col=oce.colors.jet(100)[rescale(x, c(1, 100))])
}

\author{Dan Kelley}

\keyword{misc}
