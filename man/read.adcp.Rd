\name{read.adcp}

\alias{read.adcp}

\title{Read an ADCP data file}

\description{Read an ADCP data file, producing an object of type \code{adcp}.}

\usage{read.adcp(file, type="RDI", skip=0, read, stride=1,
          debug=0, monitor=TRUE, log.action)
}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to load.}
  \item{type}{if \code{NULL}, then the first line is studied, in order
    to determine the file type.  (Ignored at present.)}
  \item{skip}{optional number of profiles to skip at start.}
  \item{read}{optional number of profiles to read.}
  \item{stride}{a number indicating the stride length to use,
    while reading the file.  If \code{stride}>1, then \code{stride-1}
    profiles are skipped, between every profile read.}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
    moderate amount of debugging information, or to 2 to get more.} 
  \item{monitor}{boolean, set to \code{TRUE} to provide an indication
    (with numbers and dots) of every profile read.}
  \item{log.action}{if provided, the action item to be stored in the
    log.  (Typically only provided for internal calls; the default that
    it provides is better for normal calls by a user.)}
}

\details{Reads a binary-format ADCP file.  The file structure is
  explained in Chapter 5 of \emph{Teledyne-RDI} (2007).

  There are three types of element stored in the result's \code{data},
  namely space-series, time-series, and matrix.  These are contained
  within lists named \code{data$ss}, \code{data$ts} and \code{data$ma},
  and their contents are as follows.

  \enumerate{
    
    \item{\code{ss}}{A space-series named \code{data$ss$distance}, which
      stores the distance of cells from the transducer head, in the
      vertical (not slant-wise) direction.}

    \item{\code{ts}}{A set of time-series made up of values for the
      profiles: \code{data$ts$time} (POSIXct time of profile),
      \code{data$ts$pressure} (pressure at the sensor head),
      \code{data$ts$temperature} (temperature at the sensor head, in deg
      C), \code{data$ts$salinity} (the salinity in
      PSU, if measured, or a constant otherwise),
      \code{data$ts$depth.of.transducer}
      (depth of the transducer), \code{data$ts$heading} (heading of the
      instrument package), \code{data$ts$pitch}
      (as \code{data$ts$heading}, but the pitch), \code{data$ts$roll}
      (as \code{data$ts$heading}, but the roll).}

    \item{\code{ma}}{A set of matrices, with rows corresponding to
      profiles, and columns corresponding to ADCP cells.  These matrices
      store velocities along the individual beams (\code{data$ma$bm1},
      \code{data$ma$bm2}, \code{data$ma$bm3}, and \code{data$ma$bm4}),
      echo intensity data (\code{data$ma$ei1}, \code{data$ma$ei2},
      \code{data$ma$ei3}, and \code{data$ma$ei4}) and a percent-good
      flag (\code{data$ma$pg1}, \code{data$ma$pg2}, \code{data$ma$pg3},
      and \code{data$ma$pg4}).  Note: the beam data get replaced with
      frame-based velocity data (u,v,w,e) by
      \code{\link{adcp.beam2frame}}, and then with earth-based velocity
      data (east, north, up, e) by \code{\link{adcp.frame2earth}}.}  }

  In addition to this \code{data} entry, the \code{metadata} entry holds
  a wealth of information as well, e.g. on beam geometry, etc.

}

\value{An object of \code{\link[base]{class}} \code{"adcp"}, which
  contains measurements made with an ADCP device.  The value of
  \code{metadata$coordinate} is set to \code{"beam"}, a fact that is
  used in other steps in processing.  For information on data stored in
  the object, see \dQuote{Details}.}


\seealso{The generic function \code{\link{read.oce}} provides an
  alternative to this. An \code{adcp} object may be summarized with
  \code{\link{summary.adcp}}, and plotted with \code{\link{plot.adcp}}.
}

\examples{
\dontrun{
library(oce)
adcp <- read.oce(file = "~/SL08F001.000", 
                 skip = 5040, read = 1 + 24 * 2, stride = 30 * 6)
summary(adcp)
plot(adcp)
}
}

\references{Teledyne-RDI, 2007.
\emph{WorkHorse commands and output data format.}
P/N 957-6156-00 (November 2007).
}

\author{Dan Kelley}

\keyword{misc}
