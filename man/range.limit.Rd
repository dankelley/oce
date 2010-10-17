\name{rangelimit}

\alias{rangelimit}

\title{Substitute NA for data outside a range}

\description{Substitute NA for data outside a range, e.g. to remove wild
spikes in data.}

\usage{rangelimit(x, min, max)}

\arguments{
  \item{x}{vector of values}
  \item{min}{minimum acceptable value.  If not supplied, and if
    \code{max} is also not supplied, a \code{min} of the 0.5 percentile
    will be used.}
  \item{max}{maximum acceptable value.  If not supplied, and if
    \code{min} is also not supplied, a \code{min} of the 0.995 percentile
    will be used.}
}

\examples{
ten.to.twenty <- rangelimit(1:100, 10, 20)
}

\author{Dan Kelley}

\keyword{misc}
