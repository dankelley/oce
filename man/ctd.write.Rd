\name{ctd.write}
\alias{ctd.write}
\title{Write a CTD data object as a .cnv file}
\description{
  Write a CTD data object as a \code{.cnv} file.
}
\usage{
ctd.write(object, file=stop("'file' must be specified"))
}
\arguments{
  \item{object}{A \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{file}{Either a character string (the file name) or a connection.  This is a mandatory argument.}
}
\details{
  Writes a file in a format as for reading by \code{read.ctd}.
}
\value{
}

\seealso{
  \code{\link{read.ctd}}.
}
\examples{
library(oce)
data(ctd)
ctd.write(ctd, "ctd.cnv")
}
\references{
  The Seabird CTD instrument is described at
  \url{http://www.seabird.com/products/spec_sheets/19plusdata.htm}.
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}
