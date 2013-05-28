\name{read.echosounder}
\alias{read.echosounder}

\title{Read an echosounder data file}

\description{Read an echosounder data file, producing an object of type \code{echosounder}.}

\usage{read.echosounder(file, channel=1, soundSpeed=swSoundSpeed(35,10,50),
                 tz=getOption("oceTz"), debug=getOption("oceDebug"))
}

\arguments{

  \item{file}{a connection or a character string giving the name of the file to
      load.}

  \item{channel}{sequence number of channel to extract, for multi-channel files.}

  \item{soundSpeed}{sound speed, in m/s.  (The documents on Biosonics
      instruments suggest that this could be inferred from values in the file
      header, but test files proved this to be false.)}

  \item{tz}{character string indicating time zone to be assumed in the data.}

  \item{debug}{a flag that turns on debugging.  Set to 1 to get a moderate
      amount of debugging information, or to 2 to get more.}

}

\details{Reads a single-beam biosonics echosounder file.  This function was
written for and tested with Biosonics files of type V3, and may not work
properly with other file formats.}

\value{An object of \code{\link[base]{class}} \code{"echosounder"} with
    standard slots \code{metadata}, \code{data} and \code{processingLog} that
    are described in the documentation for the object
    \code{\link{echosounder-class}}.}

\seealso{The documentation for \code{\link{echosounder-class}} explains the
    structure of \code{ctd} objects, and also outlines the other functions
    dealing with them.}

\examples{
library(oce)
\dontrun{
e <- read.echosounder('data.dt4')
plot(e)
}
}
    
\section{Bugs}{This function is designed to work with single-beam instruments.
Dual-beam and split-beam data are not decoded correctly, with every second
datum in the vertical being incorrect. This means that plots of dual- and
split-beam data will have a great deal of noise, often so much as to hide any
patterns of interest.  (There are plans to remove this bug during the summer of
2013.)

Also, there are several time items in the data format, and it remains to be
seen whether the one used here is appropriate.}

\references{
    Various echousounder instruments provided by BioSonics are described at the
    company website, \url{http://www.biosonicsinc.com/}.  The following document

    Biosonics, 2010.  DT4 Data File Format Specification.  BioSonics Advanced
    Digital Hydroacoustics. July, 2010.  SOFTWARE AND ENGINEERING LIBRARY
    REPORT BS&E-2004-07-0009-2.0.  (This document was provided to the author of
    this function in November 2011, which suggests that the data format was not
    changed since July 2010.)}

\author{Dan Kelley}

\keyword{misc}
