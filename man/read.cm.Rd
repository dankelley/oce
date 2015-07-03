% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{read.cm}
\alias{read.cm}
\alias{read.cm.s4}

\title{Read a current-meter data file}

\description{Read a current-meter data file, producing an object of type
  \code{cm}.}

\usage{read.cm(file, from=1, to, by=1, tz=getOption("oceTz"),
        type=c("s4"),
        longitude=NA, latitude=NA,
        debug=getOption("oceDebug"), monitor=FALSE, processingLog, \dots)
}

\arguments{
  \item{file}{a connection or a character string giving the name of the file to
    load.}

  \item{from}{index number of the first measurement to be read, or the time of
    that measurement, as created with \code{\link{as.POSIXct}} (hint: use
    \code{tz="UTC"}).}

  \item{to}{indication of the last measurement to read, in a format matching
    that of \code{from}.}

  \item{by}{an indication of the stride length to use while walking
    through the file. If this is an integer, then \code{by-1}
    measurements are skipped between each pair of profiles that is
    read. This may not make much sense, if the data are not equi-spaced
    in time.  If \code{by} is a string representing a time interval, in
    colon-separated format, then this interval is divided by the
    sampling interval, to get the stride length. \emph{BUG:} if the data
    are not equi-spaced, then odd results will occur.}

  \item{longitude}{optional signed number indicating the longitude in degrees
    East.}

  \item{latitude}{optional signed number indicating the latitude in degrees
    North.}

  \item{type}{character string indicating type of file (ignored at present).}

  \item{tz}{character string indicating time zone to be assumed in the data.}

  \item{debug}{a flag that turns on debugging.  The value indicates the depth
    within the call stack to which debugging applies.}

  \item{monitor}{ignored at present.}

  \item{processingLog}{if provided, the action item to be stored in the log.
    This parameter is typically only provided for internal calls; the default
    that it provides is better for normal calls by a user.}

  \item{\dots}{optional arguments passed to plotting functions.}

}

\details{This function has been tested on only a single file, and the
  data-scanning algorithm was based on visual inspection of that file.  Whether
  it will work generally is an open question.}

\value{An object of \code{\link[base]{class}} \code{"cm"}, which contains
  measurements made with a current-meter device.  For information on data stored
  in the object, see \dQuote{Details}.}

\seealso{The documentation for \code{\link{cm-class}} explains the structure of
  \code{cm} objects, and also outlines the other functions dealing with them.}

\examples{
\dontrun{
library(oce)
cm <- read.oce("cm_interocean_0811786.s4a.tab")
summary(cm)
plot(cm)
}
}

\author{Dan Kelley}

\keyword{misc}
