\name{read.adcp}

\alias{read.adcp}

\title{Read an ADCP data file}

\description{Read an ADCP data file, producing an object of type \code{adcp}.}

\usage{read.adcp(file, type="RDI", skip=0, read, debug=0, log.action)
}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to load.}
  \item{type}{if \code{NULL}, then the first line is studied, in order
    to determine the file type.  (Ignored at present.)}
  \item{skip}{optional number of profiles to skip at start}
  \item{read}{optional number of profiles to read}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a moderate
    amount of debugging information, or to 2 to get more.}
  \item{log.action}{if provided, the action item to be stored in the
    log.  (Typically only provided for internal calls; the default that
    it provides is better for normal calls by a user.)}
}

\details{Reads a binary-format ADCP file.}

\value{An object of \code{\link[base]{class}} \code{"adcp"}, which is a
  list with elements described below.

  (FILL IN DETAILS LATER.)
}

\seealso{The generic function \code{\link{read.oce}} provides an
  alternative to this.}

\examples{
\dontrun{
library(oce)
x <- read.oce("mooring-A")
}
}

\references{The file format seems to be described only in files that are
  non-public (restricted to those who register on the RDI website), so
  no references can be provided here.}

\author{Dan Kelley}

\keyword{misc}
