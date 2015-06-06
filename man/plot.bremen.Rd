\name{plot.bremen}

\alias{plot.bremen}
\alias{plot,bremen,missing-method}
\alias{plot,bremen-method}

\title{Plot a bremen object}

\description{Plot a \code{bremen} object}

\usage{\S4method{plot}{bremen}(x, type, \dots)}

\arguments{
  \item{x}{A \code{bremen} object, e.g. as read by \code{\link{read.bremen}}.}

  \item{type}{Optional string indicating the type to which \code{x} should be
      coerced before ploting. The choices are \code{ctd} and \code{ladp}.}

  \item{\dots}{Other arguments, passed to plotting functions.}
}

\details{If \code{x} seems to be a CTD dataset, uses \code{\link{plot.ctd}};
    otherwise, \code{x} is assumed to be a lowered-adp object, and a two-panel
    plot is created with \code{\link{plot.ladp}} to show velocity varation with
    pressure.}

\author{Dan Kelley}

\keyword{hplot}
