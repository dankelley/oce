\name{subset.drifter}

\alias{subset.drifter}
\alias{subset,drifter,missing-method}
\alias{subset,drifter-method}

\title{Subset a drifter object}

\description{Subset a drifter object}

\usage{\S4method{subset}{drifter}(x, subset, \dots)}

\arguments{
  \item{x}{a \code{drifter} object.}
  \item{subset}{a condition to be applied to the \code{data} portion of
    \code{x}.  See \sQuote{Details}.}
  \item{\dots}{ignored.}
}

\details{This function is somewhat analogous to
    \code{\link{subset.data.frame}}, but only one independent variable may be
    used in \code{subset} in any call to the function, which means that
    repeated calls will be necessary to, subset based on more than one
    independent variable.  Subsetting may be by \code{time}, \code{latitude},
    \code{longitude}, or \code{profile}.}

\value{A new \code{drifter} object.}

\examples{
library(oce)
data(drifter)
par(mfrow=c(2,2))
plot(drifter)
plot(subset(drifter, time > mean(time)))
plot(subset(drifter, longitude > mean(longitude)))
plot(subset(drifter, latitude > mean(latitude)))
}

\author{Dan Kelley}

\keyword{misc}
