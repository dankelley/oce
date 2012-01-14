\name{summary.drifter}

\alias{summary.drifter}

\title{Summarize a drifter object}

\description{Summarizes some of the data in a \code{drifter} object.}

\usage{
\method{summary}{drifter}(object, \dots)
}

\arguments{
  \item{object}{an object of class \code{"drifter"}, usually, a result of a
    call to \code{\link{read.drifter}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented.}

\value{An object of class \code{"summary.drifter"}, which contains
  pertinent information about the CTD record and its processing.}

\seealso{The documentation for \code{\link{drifter-class}} explains the
    structure of drifter objects, and also outlines the other functions dealing
    with them.}

\examples{
\dontrun{
library(oce)
d <- read.drifter("/data/OAR/6900388_prof.nc")
summary(d)
}
}

\references{\url{http://www.argo.ucsd.edu/}}

\author{Dan Kelley}

\keyword{misc}
