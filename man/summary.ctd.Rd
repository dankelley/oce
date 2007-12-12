\name{summary.ctd}
\alias{summary.ctd}
\title{Summarize a seawater (CTD) data object}
\description{
  Summarizes some of the data in a CTD object.
}
\usage{
\method{summary}{ctd}(object, \dots)
}
\arguments{
  \item{object}{A \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.}
}
\details{
  Pertinent summary information is presented, including the sampling
  location, data ranges, etc.
}
\value{
  NULL
}

\seealso{
A \code{ctd} object may be read with \code{\link{read.ctd}}
or created with \code{\link{as.ctd}}.
}
\examples{
\dontrun{
library(oce)
profile <- read.ctd("/usr/local/lib/R/library/oce/demo/ctdprofile.cnv")
summary.ctd(profile)
summary(profile)
}
}
\references{
  The Seabird CTD instrument is described at
  \url{http://www.seabird.com/products/spec_sheets/19plusdata.htm}.
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}
