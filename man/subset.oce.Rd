\name{subset.oce}

\alias{subset.oce}
\alias{subset,oce,missing-method}
\alias{subset,oce-method}

\title{Subset an oce object}

\description{Subset an oce object}

\usage{\S4method{subset}{oce}(x, subset, \dots)}

\arguments{
  \item{x}{a \code{oce} object.}

  \item{subset}{logical expression that is expressed in terms of the
      \code{data} slot of \code{x}.  All elements of \code{data} are indexed by
      the result of the expression. See \dQuote{Examples}.}

  \item{\dots}{ignored.}
}

\details{This function is somewhat analogous to
    \code{\link{subset.data.frame}}.}

\examples{
library(oce)
data(ctd)
# Select just the top 10 metres (pressure less than 10 dbar)
top10 <- subset(ctd, pressure < 10)
par(mfrow=c(1, 2))
plotProfile(ctd)
plotProfile(top10)
}

\value{A new \code{oce} object.}

\author{Dan Kelley}

\keyword{misc}
