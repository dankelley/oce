\name{subset.argo}

\alias{subset.argo}
\alias{subset,argo,missing-method}
\alias{subset,argo-method}

\title{Subset an Argo object}

\description{Subset an Argo object}

\usage{\S4method{subset}{argo}(x, subset, \dots)}

\arguments{
  \item{x}{a \code{argo} object.}
  \item{subset}{a condition to be applied to the \code{data} portion of
    \code{x}.  See \sQuote{Details}.}
  \item{\dots}{ignored.}
}

\details{This function is somewhat analogous to
    \code{\link{subset.data.frame}}, but only one independent variable
    may be used in \code{subset} in any call to the function, which
    means that repeated calls will be necessary to subset based on more
    than one independent variable.  Subsetting may be by \code{time},
    \code{latitude}, \code{longitude}, \code{profile}, \code{dataMode},
    or \code{pressure}. Note that to subset by \code{pressure}, the argo
    must be gridded to regular pressure levels (e.g. using
    \code{\link{argoGrid}}).}

\value{A new \code{argo} object.}

\examples{
library(oce)
data(argo)
par(mfrow=c(2,2))
plot(argo)
plot(subset(argo, time > mean(time)))
plot(subset(argo, longitude > mean(longitude)))
plot(subset(argoGrid(argo), pressure > 500 & pressure < 1000), which=5)

## Use only delayed-mode profiles
plot(subset(argo, dataMode == "D"))
}

\seealso{\code{\link{argoGrid}} for gridding argo objects to
  regular pressure levels.}

\author{Dan Kelley}

\keyword{misc}
