\name{summary.drifter}

\alias{summary.drifter}
\alias{print.summary.drifter}

\title{Summarize a drifter object}

\description{Summarizes some of the data in a \code{drifter} object.}

\usage{
\method{summary}{drifter}(object, \dots)

\method{print}{summary.drifter}(x, digits=max(6, getOption("digits")-1), \dots)
}

\arguments{
  \item{object}{an object of class \code{"drifter"}, usually, a result of a
    call to \code{\link{read.drifter}}.}
  \item{x}{an object of class \code{"summary.drifter"}, usually, a result of a
    call to \code{summary.drifter}.}
  \item{digits}{the number of significant digits to use when printing.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented.}

\value{An object of class \code{"summary.drifter"}, which contains
  pertinent information about the CTD record and its processing.}

\seealso{A \code{drifter} object may  be read with \code{\link{read.drifter}}.}

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
