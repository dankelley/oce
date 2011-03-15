\name{adp.xyz2enu}

\alias{adp.xyz2enu}

\title{Convert ADP from xyz coordinates to enu coordinates}

\description{Convert ADP velocity components from a xyz-based
  coordinate system to an enu-based coordinate system, by using
  the instrument's recording of heading,  pitch, and roll.}

\usage{adp.xyz2enu(x, declination=0, debug=getOption("oce.debug"))}

\arguments{
  \item{x}{an object of class \code{"adp"}.}
  \item{declination}{magnetic declination to be added to the heading after
      "righting" (see below), to get ENU with N as "true" north.}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a moderate
      amount of debugging information, or to 2 to get more.} }

\details{A single formula is used for different instruments, but its
    constituents are altered on a case-by-case basis.  The first step is to
    convert the (x,y,z) velocity components (stored in the three columns of
    \code{x$data$ma$v[,,1:3]}) into what RDI (1998 pages 11 and 12) calls
    "ship", or "righted" components.  For example, the z coordinate may point
    upwards or downwards, depending on the instrument configuration, and so
    this is converted to a so-called "mast" coordinate that points upwards (as
    for a righted ship).  The other ship coordinates are called "starboard" and
    "forward", and they take the meanings that a mariner would expect.
   
    Once the (x,y,z) velocities are converted to ship velocities, the detailed
    orientation of the instrument is extracted from \code{x$data$ts$heading},
    \code{x$data$ts$pitch}, and \code{x$data$ts$roll}.  As in the previous
    step, these are adjusted to account for instrument type and upward/downward
    orientation.
    
    After these steps, a rotation matrix is used to convert to east-north-up
    coordinates.  This is as described in RDI (1998, pages 13 and 14), which is
    an excellent resource for further reading, in general.
}

\value{An object with \code{data$ma$v[,1:3,]} altered appropriately,
  and \code{metadata$oce.orientation} changed from \code{xyz} to
  \code{enu}.}

\references{RD Instruments, 1998.
\emph{ADCP Coordinate Transformation, formulas and calculations.}
P/N 951-6079-00 (July 1998).}

\seealso{See \code{\link{read.adp}} for other functions that relate to objects
of class \code{"adp"}.}

\author{Dan Kelley}

\keyword{misc}
