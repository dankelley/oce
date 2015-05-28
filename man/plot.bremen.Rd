\name{plot.bremen}

\alias{plot.bremen}
\alias{plot,bremen,missing-method}
\alias{plot,bremen-method}

\title{Plot a bremen object (TEMPORARY code)}

\description{Plot a bremen object (TEMPORARY code)}

\usage{\S4method{plot}{bremen}(x)}

\arguments{
  \item{x}{A \code{bremen} object, e.g. as read by \code{\link{read.bremen}}.}
}

\details{If \code{x} seems to be a CTD dataset, uses \code{\link{plot.ctd}};
    otherwise, \code{x} is assumed to be a lowered-adcp object, and a
    four-panel plot is created to show velocity and shear components over
    depth.}

\author{Dan Kelley}

\keyword{hplot}
