\name{plot.ladp}

\alias{plot.ladp}
\alias{plot,ladp,missing-method}
\alias{plot,ladp-method}

\title{Plot an ladp object}

\description{Plot data contained within an lap object.}

\usage{\S4method{plot}{ladp}(x)}

\arguments{
  \item{x}{A \code{ladp} object, e.g. as read by \code{\link{as.ladp}}.}
}

\details{Uses \code{\link{plotProfile}} to create panels of depth variation of
easterly and northerly velocity components.}

\seealso{The documentation for \code{\link{ladp-class}} explains the structure
    of ladp objects, and also outlines the other functions dealing with them.}

\author{Dan Kelley}

\keyword{hplot}
