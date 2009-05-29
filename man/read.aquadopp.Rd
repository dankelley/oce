\name{read.aquadopp}

\alias{read.aquadopp}

\title{Read an AQUADOPP data file}

\description{Read an AQUADOPP data file, producing an object of type \code{aquadopp}.}

\usage{read.aquadopp(file, type=c("nortek aquadopp high resolution"),
                     skip=0, read, stride=1,
                     debug=0, monitor=TRUE, log.action)
}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to load.}
  \item{type}{type of instrument.  At present, only one type of
    instrument is handled.}
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

\details{Reads a binary-format AQUADOPP file from NorTek.

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
      profiles, and columns corresponding to AQUADOPP cells.  These matrices
      store velocities along the individual beams (\code{data$ma$v1},
      \code{data$ma$v2}, and \code{data$ma$v3}),
      amplitude (\code{data$ma$a1}, \code{data$ma$a2},
      \code{data$ma$a3}) and correlation 
      (\code{data$ma$q1}, \code{data$ma$cq}, \code{data$ma$q3}).}

  In addition to this \code{data} entry, the \code{metadata} entry holds
  a wealth of information as well, e.g. on beam geometry, etc.

}
}

\value{An object of \code{\link[base]{class}}es \code{"aquadopp"} and \code{"adcp"}.}

\seealso{An \code{aquadopp} object may be summarized with
  \code{\link{summary.adcp}}, and plotted with \code{\link{plot.adcp}},
  both of which are generic functions.}

\examples{
\dontrun{
library(oce)
d <- read.aquadopp("sl08AQ01.prf", skip=1080, read=500)
plot(d, which=15, ylim=c(0,10))
abline(v=as.POSIXct("2008-06-25 13:00:00", tz="UTC"),col="red")
}
}

\references{General information on the aquadopp profiler, and other
  NorTek products, is available at \url{http://www.nortekusa.com/}.}

\author{Dan Kelley}

\keyword{misc}
