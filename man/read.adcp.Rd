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
  explained in Chapter 5 of \emph{Teledyne-RDI} (2007).


  There are three types of data stored in the result's \code{data}
  entry.  

  \enumerate{
    
    \item{1.}{A single vector named \code{data$distance}, which stores
      the distance of cells from the transducer head, in the vertical
      (not slant-wise) direction.}

    \item{2.}{A set of vectors that have one element per profile:
      \code{data$time} (the POSIXct time of the profile),
      \code{data$pressure} (the pressure at the sensor head),
      \code{data$temperature} (the temperature at the sensor head, in
      deg C), \code{data$salinity} (the salinity at the sensor head, in
      PSU ... often, not actually measured, but rather set to a constant
      value for the experiment), \code{data$depth.of.transducer} (depth
      of the transducer), \code{data$heading} (heading of the instrument
      package, a value used by \code{\link{adcp.frame2earth}} for
      converting to earth-based coordinates), \code{data$pitch} (as
      \code{data$heading}, but the pitch), \code{data$roll} (as
      \code{data$heading}, but the roll).}

    \item{3.}{The third type is a set of matrices, with rows
      corresponding to profiles, and columns corresponding to ADCP
      cells.  These matrices store velocities along the individual beams
      (\code{data$bm1}, \code{data$bm2}, \code{data$bm3}, and
      \code{data$bm4}), echo intensity data (\code{data$ei1},
      \code{data$ei2}, \code{data$ei3}, and \code{data$ei4}) and a
      percent-good flag (\code{data$pg1}, \code{data$pg2},
      \code{data$pg3}, and \code{data$pg4}).}

  }

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
b <- read.oce(file = "~/SL08F001.000", skip = 5000, read = 6 * 30)
summary(b)
plot(b)
}
}

\references{Teledyne-RDI, 2007.
\emph{WorkHorse commands and output data format.}
P/N 957-6156-00 (November 2007).
}

\author{Dan Kelley}

\keyword{misc}
