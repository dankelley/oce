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
    \item{rlow}{value of the result corresponding to \code{x} equal to
        \code{xlow}.}
    \item{rhigh}{value of the result corresponding to \code{x} equal to
        \code{xhigh}.}
    \item{clip}{logical, set to \code{TRUE} to clip the result to the range
        spanned by \code{rlow} and \code{rhigh}.}
}

\details{This is helpful in e.g. developing a color scale for an image plot.
    It is not necessary that \code{rlow} be less than \code{rhigh}, and in fact
    reversing them is a good way to get a reversed colorscale for a plot.}

\value{A new vector, which has minimum \code{lim[1]} and maximum \code{lim[2]}.}

\examples{
low <- 0
high <- 1
x <- runif(1000, min=low, max=high)
y <- runif(1000, min=low, max=high)
z <- sqrt(0.5 * (x^2+y^2))
# "jet" color scheme
drawPalette(zlim=c(low,high), col=oceColorsJet)
col <- oceColorsJet(256)[rescale(z, xlow=low, xhigh=high, rlow=1, rhigh=256)]
plot(x, y, col=col, cex=4, pch=20, asp=1)
contour <- interpBarnes(x,y,z)
contour(contour$xg, contour$yg, contour$zg, add=TRUE, labcex=1, levels=seq(0,1,0.2))

# forward and reverse "jet" colorscale
i <- 1:100
plot(i, rep(0, 100), pch=20, cex=2, col=oceColorsJet(100)[rescale(i, rlow=1, rhigh=100)])
points(i, rep(1, 100), pch=20, cex=2, col=oceColorsJet(100)[rescale(i, rlow=100, rhigh=1)])
}

\author{Dan Kelley}

\keyword{misc}
