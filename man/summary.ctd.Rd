\name{summary.ctd}

\alias{summary.ctd}

\title{Summarize a seawater (CTD) data object}

\description{Summarizes some of the data in a \code{ctd} object.}

\usage{\method{summary}{ctd}(object, \dots)}

\arguments{
  \item{object}{A \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{\dots}{passed to children.}
}

\details{Pertinent summary information is presented, including the
  sampling location, data ranges, etc.}

\value{None}

\seealso{
A \code{ctd} object may be read with \code{\link{read.ctd}}
or created with \code{\link{as.ctd}}.
}

\examples{
library(oce)
data(ctd)
#ctd <- read.ctd("ctdprofile.cnv")
summary(ctd)
}

\references{The Seabird CTD instrument is described at
  \url{http://www.seabird.com/products/spec_sheets/19plusdata.htm}.
}

\author{Dan Kelley}

\keyword{misc}
