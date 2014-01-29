\name{ctdUpdateHeader}
\alias{ctdUpdateHeader}
\title{Update a CTD header}
\description{Update the header of a \code{ctd} object}
\usage{ctdUpdateHeader(x, debug=FALSE)}
\arguments{
  \item{x}{A \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{debug}{Set to \code{TRUE} for debugging.}
}

\details{Update the header of a \code{ctd} object, e.g. adjusting
  \code{nvalues} and the \code{span} of each column. This is done
  automatically by \code{ctdTrim}, for example.
}

\value{A new \code{ctd} object.}

\seealso{The documentation for \code{\link{ctd-class}} explains the structure
    of CTD objects, and also outlines the other functions dealing with them.}

\examples{
library(oce)
data(ctd) 
ctd[["pressure"]] <- ctd[["pressure"]] + 3
ctdNew <- ctdUpdateHeader(ctd)
}

\references{The Seabird CTD instrument is described at
  \url{http://www.seabird.com/products/spec_sheets/19plusdata.htm}.
}

\author{Dan Kelley}
\keyword{misc}
