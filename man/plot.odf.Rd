\name{plot.odf}

\alias{plot.odf}
\alias{plot,odf,missing-method}
\alias{plot,odf-method}

\title{Plot an ODF object}

\description{Plot data contained within an ODF object.}

\usage{\S4method{plot}{odf}(x)}

\arguments{
  \item{x}{A \code{odf} object, e.g. as read by \code{\link{read.odf}}, or a
      list containing items named \code{salinity} and \code{temperature}.}
}

\details{Uses \code{\link{oce.plot.ts}} to create panels of time-series plots
    for all the columns contained in the \code{odf} object.}

\seealso{The documentation for \code{\link{odf-class}} explains the structure
    of CTD objects, and also outlines the other functions dealing with them.}

\author{Dan Kelley}

\keyword{hplot}
