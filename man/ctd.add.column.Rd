\name{ctd.add.column}
\alias{ctd.add.column}
\title{Add a column to a CTD file}
\description{
  Add a column to a CTD file, updating the header as appropriate.
}
\usage{
ctd.add.column(x, column=NULL, column.name="", 
	code="", name="", unit="", debug=FALSE)
}
\arguments{
  \item{x}{A \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{column}{A column of data.}
  \item{column.name}{The name for this column in the dataframe; e.g. if set to \code{hello}, then the column would be accessed later \code{d$data$hello}.}
  \item{code}{Item to put before the \code{:} in the header line.}
  \item{name}{Item to put after the \code{:}, but before the \code{[]}.}
  \item{unit}{Item inside the \code{[]} in the header line.}
  \item{debug}{Set \code{TRUE} to see information about the processing.}
}
\details{
  This adds a line of the form \preformatted{* name N = code: name [unit]} 
  within the header list, and also adds the data column itself.
  You should study an existing \code{.cnv} file to see what
  sort of format to use, to avoid confusion if you share the resultant
  file.
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
sigthe <- sw.sigma.theta(ctd$data$salinity,
	ctd$data$temperature,
	ctd$data$pressure);
ctd.new <- ctd.add.column(ctd,
	sigthe, "sigmatheta",
	"sigma-theta", "density, sigma-theta",
	"kg/m^3");
}
\references{
  The Seabird CTD instrument is described at
  \url{http://www.seabird.com/products/spec_sheets/19plusdata.htm}.
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}
