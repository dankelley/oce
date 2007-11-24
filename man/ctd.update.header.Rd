\name{ctd.update.header}
\alias{ctd.update.header}
\title{Update a CTD header}
\description{
  Update the header of a CTD object
}
\usage{
ctd.update.header(x, debug=FALSE)
}
\arguments{
  \item{x}{A \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{debug}{Set to \code{TRUE} for debugging.}
}
\details{
  Update the header of a CTD object, e.g. adjusting \code{nvalues} and
  the \code{span} of each column. This is done automatically 
  by \code{ctd.trim}, for example.
}
\value{
  A new \code{ctd} object.
}
\seealso{
\code{read.ctd} reads a CTD object, and \code{write.ctd} writes one.
}
\examples{
library(oce)
data(ctd) 
ctd$data$p <- ctd$data$p + 3 # adjust pressures
ctd.new <- ctd.update.header(ctd)
}
\references{
  The Seabird CTD instrument is described at
  \url{http://www.seabird.com/products/spec_sheets/19plusdata.htm}.
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}
