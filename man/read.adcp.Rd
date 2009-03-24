\name{read.adcp}

\alias{read.adcp}

\title{Read an ADCP data file}

\description{Read an ADCP data file, producing an object of type \code{adcp}.}

\usage{read.adcp(file, type="RDI", skip=0, read, 
          debug=0, monitor=TRUE, log.action)
}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to load.}
  \item{type}{if \code{NULL}, then the first line is studied, in order
    to determine the file type.  (Ignored at present.)}
  \item{skip}{optional number of profiles to skip at start.}
  \item{read}{optional number of profiles to read.}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
    moderate amount of debugging information, or to 2 to get more.} 
  \item{monitor}{boolean, set to \code{TRUE} to provide an indication
    (with numbers and dots) of every profile read.}
  \item{log.action}{if provided, the action item to be stored in the
    log.  (Typically only provided for internal calls; the default that
    it provides is better for normal calls by a user.)}
}

\details{Reads a binary-format ADCP file.  The file structure is
explained in Chapter 5 of Teledyne-RDI (2007).}

\value{An object of \code{\link[base]{class}} \code{"adcp"}, which
  contains measurements made with an ADCP device.}

\seealso{The generic function \code{\link{read.oce}} provides an
  alternative to this. An \code{adcp} object may be summarized with
  \code{\link{summary.adcp}}, and plotted with \code{\link{plot.adcp}}.
}

\examples{
\dontrun{
library(oce)
d <- read.oce("~/SL08F001.000",read=60)
summary(d)
plot(d)
}
}

\references{Teledyne-RDI, 2007.
WorkHorse commands and output data format.
P/N 957-6156-00 (November 2007).
}

\author{Dan Kelley}

\keyword{misc}
