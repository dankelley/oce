\name{read.adv}
\alias{read.adv}
\alias{read.adv.sontek}
\alias{read.adv.nortek}

\title{Read an ADV data file}

\description{Read an ADV data file, producing an object of type \code{adv}.}

\usage{
read.adv(file, from=0, to, by=1,
        type=c("nortek", "sontek"),
        withHeader=TRUE, sampling.start, deltat,
        debug=0, monitor=TRUE, log.action)
read.adv.sontek(file, from=0, to, by=1,
                type="default",
                withHeader=TRUE, sampling.start, deltat,
                debug=0, monitor=TRUE, log.action)
read.adv.nortek(file, from=0, to, by=1,
                type="vector",
                withHeader=TRUE, sampling.start, deltat,
                debug=0, monitor=TRUE, log.action)
}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to load.}
  \item{from}{number of profiles to skip at start, or the POSIXt time at
    which to start reading.  See \dQuote{Examples}, and make careful
    note of the use of the \code{tz} argument. (IGNORED AT PRESENT.)}
  \item{to}{optional number of profiles to read, or the last POSIXt
    time to read. (IGNORED AT PRESENT.)}
  \item{by}{an indication of the stride length to use while walking
    through the file.  If this is an integer, then \code{by-1}
    profiles are skipped between each pair of profiles that is read.  If
    this is a string representing a time interval, in colon-separated
    format, then this interval is divided by the sampling interval, to
    get the stride length.(IGNORED AT PRESENT.)}
  \item{type}{a character string indicating the type of file.}
  \item{withHeader}{a boolean indicating whether the file has a proper 
    header or is a partial file, chopped out into
    smaller time chunks with a deck unit.}
  \item{sampling.start}{a POSIXct time corresponding to the first sample. 
    (This is mandatory if \code{withHeader=FALSE}.)}
  \item{deltat}{the time between samples.
    (This is mandatory if \code{withHeader=FALSE}.)}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
    moderate amount of debugging information, or to 2 to get more.} 
  \item{monitor}{boolean, set to \code{TRUE} to provide an indication
    (with numbers and dots) of every profile read.}
  \item{log.action}{if provided, the action item to be stored in the
    log.  (Typically only provided for internal calls; the default that
    it provides is better for normal calls by a user.)}
}

\details{Reads a binary-format ADV file.  THIS IS PRELIMINARY CODE ONLY;
  THE INTERFAE MAY CHANGE.}

\value{An object of \code{\link[base]{class}} \code{"adv"}, which
  contains measurements made with an ADV device.  For information on
  data stored in the object, see \dQuote{Details}.}


\seealso{Objects of class \code{adv} may be plotted with \code{\link{plot.adv}}
or summarized with \code{\link{summary.adv}}.}

\examples{
\dontrun{
library(oce)
adv <- read.adv("~/data/sleiwex-archive/2008/mooring/m6/autopod/day183/SL08A183.015", 
       sampling.start=as.POSIXct("2008-07-01 15:00:00", tz="UTC"), deltat=0.1)
summary(adv)
plot(adv)
}
}

\author{Dan Kelley}

\keyword{misc}
