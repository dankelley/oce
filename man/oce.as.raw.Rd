\name{oce.as.raw}

\alias{oce.as.raw}

\title{A version of as.raw() that clips data to prevent warnings}

\description{A version of as.raw() that clips data to prevent warnings}

\usage{oce.as.raw(x)}

\arguments{
    \item{x}{values to be converted to raw}
}

\details{Negative values are clipped to 0, while values above 255 are clipped
    to 255; the result is passed to \code{\link{as.raw}} and returned.}
    
\value{Raw values corresponding to \code{x}.}

\examples{
x <- c(-0.1, 0, 1, 255, 255.1)
data.frame(x, oce.as.raw(x))
}

\author{Dan Kelley}

\keyword{misc}
