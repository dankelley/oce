\name{read.adv}
\alias{read.adv}
\alias{read.adv.nortek}
\alias{read.adv.sontek}
\alias{read.adv.sontek.adr}
\alias{read.adv.sontek.text}

\title{Read an ADV data file}

\description{Read an ADV data file, producing an object of type \code{adv}.}

\usage{
read.adv(file, from=1, to, by=1,
        type=c("nortek", "sontek", "sontek.adr", "sontek.text"),
        withHeader=TRUE, sampling.start, deltat,
        tz=getOption("oce.tz"), debug=getOption("oce.debug"), monitor=TRUE, log.action)
read.adv.nortek(file, from=1, to, by=1,
                type="vector",
                withHeader=TRUE, sampling.start, deltat,
                tz=getOption("oce.tz"), debug=getOption("oce.debug"), monitor=TRUE, log.action)
read.adv.sontek(file, from=1, to, by=1,
                type="default",
                withHeader=TRUE, sampling.start, deltat,
                tz=getOption("oce.tz"), debug=getOption("oce.debug"), monitor=TRUE, log.action)
read.adv.sontek.adr(file, from=1, to, by=1,
                tz=getOption("oce.tz"), debug=getOption("oce.debug"), monitor=TRUE, log.action)
read.adv.sontek.text(basefile, from=1, to, by=1,
                     coordinate.system="xyz",
                     transformation.matrix,
                     tz=getOption("oce.tz"), debug=getOption("oce.debug"), log.action)
}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to load.}
  \item{basefile}{character string giving the base name of files to load
    (used only by read.adv.sontek.text). The actual filenames are constructed
    by appending \code{".hd1"} and \code{".ts1"} to the base name.}
  \item{from}{index number of first profile to be read, or the
    (POSIXt) time of that first profile.  See \dQuote{Examples}, and
    make careful note of the use of the \code{tz} argument.}
  \item{to}{optional number of profiles to read, or the last POSIXt
    time to read.}
  \item{by}{an indication of the stride length to use while walking
    through the file.  If this is an integer, then \code{by-1}
    profiles are skipped between each pair of profiles that is read.
    This may not make much sense, if the data are not equi-spaced in
    time.  If \code{by} is a string representing a time interval, in
    colon-separated format, then this interval is divided by the
    sampling interval, to get the stride length. \emph{BUG:} if the
    data are not equi-spaced, then odd results will occur.}
  \item{type}{a character string indicating the type of file.}
  \item{withHeader}{a boolean indicating whether the file has a proper
    header or is a partial file, chopped out into smaller time chunks
    with a deck unit.}
  \item{sampling.start}{a POSIXct time corresponding to the first sample. 
    (This is mandatory if \code{withHeader=FALSE}.)}
  \item{coordinate.system}{character string indicating coordinate system, one
    of \code{"beam"}, \code{"xyz"}, \code{"enu"} or \code{"other"}.}
  \item{transformation.matrix}{transformation matrix to use,
    over-riding anything in the data file e.g. \code{rbind(c(2.710,
    -1.409, -1.299), c(0.071, 2.372, -2.442), c(0.344, 0.344,
    0.344))}.  This is only relevant if the coordinate system is
    \code{"beam"}.}
  \item{tz}{character string indicating time zone to be assumed in the data.}
  \item{deltat}{the time between samples. (This is mandatory if
    \code{withHeader=FALSE}.)}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
    moderate amount of debugging information, or to 2 to get more.  Note
    that \code{debug} is reduced by 1 for every depth in the function
    call, i.e. \code{read.adv.nortek()} calls
    \code{read.header.nortek()}, so you should set \code{debug=2} to get
    debugging to "reach" down to the level of reading the header.}
  \item{monitor}{boolean, set to \code{TRUE} to provide an indication
    (with numbers and dots) of every profile read.}
  \item{log.action}{if provided, the action item to be stored in the
    log.  This parameter is typically only provided for internal
    calls; the default that it provides is better for normal calls by
    a user.}
}

\details{Reads a binary-format ADV file.

  It is crucial to pay careful attention if \code{withHeader} is
  \code{FALSE}, since that condition forces \code{read.adv} to make
  guesses about important things.  In particular, it will set
  \code{metadata$deltat=1},
  \code{metadata$orientation="downward"} and
  \code{metadata$oce.coordinate="beam"}.  It is up to the user to
  modify these values if they are wrong.

  Special notes:
  
  Nortek adr files: the file format assumed here is not the same as in the 
  documentation that I have available.  The documentation is for Firmware 
  Version 4.0 (October 1997), but this does not match a data file used for testing.
  The following are the differences:
  \enumerate{
    \item The second byte of the burst header is 0x11, but the
    documentation says it should be 0x10; the code takes either.
    \item The data length is assumed to be 22, no matter it is.  This
    is done to match the files being used by the author.  The
    documentation suggests that the number of bytes can be inferred
    from the header, but this is difficult because the information
    stored in the header \code{RecordedData} byte does not agree with
    the docs (bit 0 is 0 in the file, but we *do* have amplitude and
    correlation data).
    \item The sampling rate is taken to be in units of 0.01Hz, not 0.1Hz as documented.
  }
}

\value{An object of \code{\link[base]{class}} \code{"adv"}, which
  contains measurements made with an ADV device.  For information on
  data stored in the object, see \dQuote{Details}.}

\seealso{Objects of class \code{adv} may be plotted with
  \code{\link{plot.adv}} or summarized with \code{\link{summary.adv}}.
  Coordinate transformations are done with \code{\link{adv.beam2xyz}},
  \code{\link{adv.xyz2enu}}, and \code{\link{adv.enu2other}}, in
  that order.}

\examples{
\dontrun{
library(oce)
adv <- read.adv("/data/archive/sleiwex/2008/moorings/m06/autopod/day183/SL08A183.015",
       sampling.start=as.POSIXct("2008-07-01 15:00:00", tz="UTC"), deltat=0.1)
summary(adv)
plot(adv)
}
}

\author{Dan Kelley}

\keyword{misc}
