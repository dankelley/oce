\name{matrix.smooth}

\alias{matrix.smooth}

\title{Smooth a matrix.}

\description{Smooth a matrix.}

\usage{matrix.smooth(m)}

\arguments{
\item{m}{a matrix to be smoothed.}
}

\value{A smoothed matrix.}

\examples{
m <- matrix(rep(seq(0, 1, length.out=5), 5), nrow=5, byrow=TRUE)
m[3,3] <- 2
m1 <- matrix.smooth(m)
m2 <- matrix.smooth(m1)
m3 <- matrix.smooth(m2)
par(mfrow=c(2,2))
image(m,  col=rainbow(100), zlim=c(0,4), main="original image")
image(m1, col=rainbow(100), zlim=c(0,4), main="smoothed 1 time")
image(m2, col=rainbow(100), zlim=c(0,4), main="smoothed 2 times")
image(m3, col=rainbow(100), zlim=c(0,4), main="smoothed 3 times")
}

\author{Dan Kelley}

\keyword{misc}
