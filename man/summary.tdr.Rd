\name{summary.tdr}

\alias{summary.tdr}

\title{Summarize an RBR temperature-depth data object}

\description{Summarize an RBR temperature-depth data object}

\usage{\method{summary}{tdr}(object, \dots)}

\arguments{
  \item{object}{A \code{tdr} object, e.g. as read by \code{\link{read.tdr}}.}
  \item{\dots}{passed to children.}
}

\details{Pertinent summary information is presented, including the
		     start and stop times, as well as statistics of
		     temperature and pressure.}

\value{None}

\seealso{See \code{\link{read.tdr}} for more information on dealing
  with the \code{tdr} data type, including examples.}

\author{Dan Kelley}

\keyword{misc}
