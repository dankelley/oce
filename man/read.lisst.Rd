% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{read.lisst}

\alias{read.lisst}

\title{Read a LISST data file}

\description{Read a LISST data file, producing an object of type \code{lisst}.}

\usage{read.lisst(file, year=0, tz="UTC", longitude=NA, latitude=NA)}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to load.}
  \item{year}{year in which the measurement of the series was made.}
  \item{tz}{time zone.}
  \item{longitude}{longitude of observation (stored in metadata)}
  \item{latitude}{latitude of observation (stored in metadata)}
}

\details{The file should contain 42 columns, with no header.  If there are fewer
  than 42 columns, an error results.  If there are more, only the first 42 are
  used.  Note that \code{\link{read.oce}} can recognize LISST files by their
  having a name ending in \code{".asc"} and by having 42 elements on the first
  line.  Even so, it is preferred to use the present function, because it gives
  the opportunity to specify the year and timezone, so that times can be
  calculated properly.}

\value{An object of \code{\link[base]{class}} \code{"lisst"}, which is a list
  with slots as detailed below. 

  \item{\code{data}}{: a list containing columns named \code{C1} to \code{C32}, 
      which are volume concentrations (in microliters per liter) for size
      classes 1 through 32, along with
      \code{lts} (laser transmission sensor, column 33),
      \code{voltage} (battery voltage in calibrated units, column 34),
      \code{aux} (external auxiliary input 1 in calibrated units, column 35),
      \code{lrs} (laster reference sensor in calibrated units, column 36),
      \code{pressure} (pressure in dbar, column 37),
      \code{temperature} (in degrees C, column 38),
      \code{dayhour} (day*100 +hour, column 39),
      \code{minutesecond} (minute*100+second, column 40),
      \code{transmission} (computed percent optical transmission over path, column 41),
      \code{beam} (computed beam-C in units of 1/m, column 42),
      and 
      \code{time}{a POSIXct time calculated from \code{dayhour} and
          \code{minuteseconds}, using the \code{year} specified as an argument,
          or the present year, if the year was not given, and using timezone
          \code{tz}.}
  }
  \item{\code{metadata}}{: a list containing the following items
    \code{filename}, the name of the file holding the data, \code{latitude}, the
    latitude of the observations, and \code{longitude}, the longitude of the
    observations.}
  \item{\code{processingLog}}{: a log of processing, in the standard \code{oce}
    format.}
}


\seealso{The documentation for \code{\link{lisst-class}} explains the structure
    of \code{lisst} objects, and also outlines the other functions dealing with
    them.  In particular, \code{read.lisst} uses \code{\link{as.lisst}} to
    create the LISST object that is returned.}

\author{Dan Kelley}

\keyword{misc}
