\name{plot.ladp}

\alias{plot.ladp}
\alias{plot,ladp,missing-method}
\alias{plot,ladp-method}

\title{Plot an ladp object}

\description{Plot data contained within an \code{ladp} object.}

\usage{\S4method{plot}{ladp}(x, which=c("u", "v"), \dots)}

\arguments{
  \item{x}{A \code{ladp} object, e.g. as read by \code{\link{as.ladp}}.}
  \item{which}{vector of names of items to be plotted.}
  \item{\dots}{Other arguments, passed to plotting functions.}
}

\details{Uses \code{\link{plotProfile}} to create panels of depth variation of
easterly and northerly velocity components.}

\seealso{The documentation for \code{\link{ladp-class}} explains the structure
    of ladp objects, and also outlines the other functions dealing with them.}

\author{Dan Kelley}

\keyword{hplot}
