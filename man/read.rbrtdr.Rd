\name{read.rbrtdr}

\alias{read.rbrtdr}

\title{Read an RBR temperature-depth recorder data file}

\description{Read an RBR temperature-depth recorder file, producing an object of type \code{rbrtdr}.}

\usage{read.rbrtdr(file, debug=FALSE, log.action)}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to load.}
  \item{debug}{a flag that can be set to \code{TRUE} to turn on debugging.}
  \item{log.action}{if provided, the action item to be stored in the
    log.  (Typically only provided for internal calls; the default that
    it provides is better for normal calls by a user.)}
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
  generic functions (see examples).  In-air samples (common at the start
  and end of an experiment) can be removed in a crude way with
  \code{\link{rbrtdr.trim}}.}}

\examples{
\dontrun{
library(oce)
d1 <- read.rbrtdr("~/sl08_011855-decim.dat")
summary(d1)
plot(d1)
# Remove in-air data
d2 <- rbrtdr.trim(d1)
d2$data$pressure <- d2$data$pressure - 10.2
d2 <- processing.log.append(d2, "subtract 10.2dbar from pressure")
plot(d2)
# Focus near the start of experiment
d3 <- subset(d2, as.POSIXct("2008-06-26") <= t & t <= as.POSIXct("2008-06-28"))
plot(d3)
}
}

\references{(fill in later ... presumably RBR has a manual online.)}

\author{Dan Kelley}

\keyword{misc}
