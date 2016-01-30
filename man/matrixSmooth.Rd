\name{matrixSmooth}

\alias{matrixSmooth}

\title{Smooth a Matrix}

\description{Smooth a matrix.}

\usage{matrixSmooth(m, passes=1)}

\arguments{
    \item{m}{a matrix to be smoothed.}
    \item{passes}{an integer specifying the number of times the smoothing is to
        be applied.}
}

\details{The values on the edge of the matrix are unaltered.  For interior points, the result is defined in terms in terms of the original as follows.
\eqn{r_{i,j} = (2 m_{i,j} + m_{i-1,j} + m_{i+1,j} + m_{i,j-1} + m_{i,j+1})/6}{r_[i,j] = (2 m_[i,j] + m_[i-1,j] + m_[i+1,j] + m_[i,j-1] + m_[i,j+1])/6}.  Note that missing values propagate to neighbours.}

\value{A smoothed matrix.}

\examples{
library(oce)
opar <- par(no.readonly = TRUE)
m <- matrix(rep(seq(0, 1, length.out=5), 5), nrow=5, byrow=TRUE)
m[3,3] <- 2
m1 <- matrixSmooth(m)
m2 <- matrixSmooth(m1)
m3 <- matrixSmooth(m2)
par(mfrow=c(2,2))
image(m,  col=rainbow(100), zlim=c(0,4), main="original image")
image(m1, col=rainbow(100), zlim=c(0,4), main="smoothed 1 time")
image(m2, col=rainbow(100), zlim=c(0,4), main="smoothed 2 times")
image(m3, col=rainbow(100), zlim=c(0,4), main="smoothed 3 times")
par(opar)
}

\author{Dan Kelley}

\keyword{misc}
