\name{subset.section}

\alias{subset.section}
\alias{subset,section,missing-method}
\alias{subset,section-method}

\title{Subset a section object}

\description{Subset a section object}

\usage{\S4method{subset}{section}(x, subset, \dots)}

\arguments{
  \item{x}{a \code{section} object.}
  \item{subset}{a condition to be applied to the \code{data} portion of
    \code{x}.  See \sQuote{Details}.}
  \item{\dots}{may include \code{debug}, to set a debugging level.}
}

\details{This function is somewhat analogous to
    \code{\link{subset.data.frame}}.  The condition set by \code{subset} may be
    in terms of \code{stationId} or any combination of \code{longitude},
    \code{latitude} and \code{time}.  However, \code{stationId} may not be
    combined with the others; to get that effect, call this function more than
    once.}

\value{A new \code{section} object.}

\examples{
library(oce)
GS <- subset(section, 109<=stationId&stationId<=129)
}

\author{Dan Kelley}

\keyword{misc}
