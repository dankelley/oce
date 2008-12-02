\name{read.rbrtdr}

\alias{read.rbrtdr}

\title{Read an RBR temperature-depth recorder data file}

\description{Read an RBR temperature-depth recorder file, producing an object of type \code{rbrtdr}.}

\usage{read.rbrtdr(file, debug=FALSE)}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to load.}
  \item{debug}{a flag that can be set to \code{TRUE} to turn on debugging.}
}

\details{Read an RBR TDR (temperature-depth recorder) file.}

\value{An object of \code{\link[base]{class}} \code{"rbrtdr"}, which is a
  list with elements detailed below. 
  The most important elements are the station name and position, along
  with the profile data that are contained in the data frame named \code{data}.
  (Other elements in the list may be deleted in future versions of the
  package, if they prove to be of little use in practice, or if they prove
  to have been idiosyncratic features of the particular files used in
  early development of \code{oce}.)
  \item{data}{a data table containing the (t, temperature, pressure) data.}
  \item{metadata}{a list containing the following items
    \item{\code{header}}{the header itself, normally containing several dozen lines of
      information.}}
  \item{processing.log}{a processing log, in the standard \code{oce} format.}
}

\seealso{The generic function \code{\link{read.oce}} provides an
alternative to this.  The object may be summarized and plotted using
generic functions (see examples).}

\examples{
\dontrun{
library(oce)
x <- read.rbrtdr("tests/RBR-01.dat")
summary(x)
plot(x) # timeseries
}
}

\references{(fill in later ... presumably RBR has a manual online.)}

\author{Dan Kelley}

\keyword{misc}
