\name{abbreviateTimeLabels}

\alias{abbreviateTimeLabels}

\title{Abbreviate a list of times by removing commonalities (e.g. year)}

\description{Abbreviate a list of times by removing commonalities (e.g. year)}

\usage{abbreviateTimeLabels(t, \dots)}

\arguments{
  \item{t}{vector of times.}
  \item{\dots}{optional arguments passed to the \code{\link{format}}, e.g. \code{format}.}
}

\details{Abbreviates time labels for purposes such as saving repetition on axes.}

\value{None.}

\seealso{This is used by various functions that draw time labels on axes, e.g.
    \code{\link{plot.adp}}.}

\author{Dan Kelley, with help from Clark Richards}

\keyword{misc}
