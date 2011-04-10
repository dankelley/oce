\name{xyz.to.enu.adp}

\alias{xyz.to.enu.adp}

\title{Convert ADP from xyz coordinates to enu coordinates}

\description{Convert ADP velocity components from a xyz-based
  coordinate system to an enu-based coordinate system, by using
  the instrument's recording of heading, pitch, and roll.}

\usage{xyz.to.enu.adp(x, declination=0, debug=getOption("oce.debug"))}

\arguments{
  \item{x}{an object of class \code{"adp"}.}
  \item{declination}{magnetic declination to be added to the heading after
      "righting" (see below), to get ENU with N as "true" north.}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a moderate
      amount of debugging information, or to 2 to get more.} }

\details{The first step is to
    convert the (x,y,z) velocity components (stored in the three columns of
    \code{x$data$ma$v[,,1:3]}) into what RDI [1, pages 11 and 12] calls
    "ship" (or "righted") components.  For example, the z coordinate, which may
    point upwards or downwards depending on instrument orientation, is mapped
    onto a "mast" coordinate that points more nearly upwards than
    downward.  The other ship coordinates are called
    "starboard" and "forward", the meanings of which will be clear to 
    mariners.  Once the (x,y,z) velocities are converted to ship
    velocities, the orientation of the instrument is extracted from
    heading, pitch, and roll vectors stored in \code{x$data$ts}.  These angles
    are defined differently for RDI and Sontek profilers.

    The code handles every case individually, based on the table given below.
    The table comes from Clark Richards, a PhD student at Dalhousie University
    [2], who developed it based on instrument documentation, discussion on user
    groups, and analysis of measurements acquired with RDI and Sontek
    acoustic current profilers in the SLEIWEX experiment [3].  In the table,
    (X, Y, Z) denote instrument-coordinate velocities, (S, F, M) denote
    ship-coordinate velocities, and (H, P, R) denote heading, pitch, and roll.

    
\tabular{rrrrrrrrrrrr}{
\strong{Case} \tab \strong{Mfr.} \tab \strong{Instr.} \strong{Orient.} \tab \strong{H}  \tab \strong{P} \tab \strong{R} \tab \strong{S} \tab \strong{F} \tab \strong{M}\cr 
1 \tab RDI    \tab  ADCP \tab    up \tab    H \tab  arctan(tan(P)*cos(R)) \tab  R \tab  -X \tab  Y \tab -Z\cr
2 \tab RDI    \tab  ADCP \tab  down \tab    H \tab  arctan(tan(P)*cos(R)) \tab -R \tab   X \tab  Y \tab  Z\cr
3 \tab Nortek \tab   ADP \tab    up \tab H-90 \tab          -P            \tab -R \tab   X \tab  Y \tab  Z\cr
4 \tab Nortek \tab   ADP \tab  down \tab H-90 \tab          -P            \tab -R \tab   X \tab  Y \tab  Z\cr
5 \tab Sontek \tab   ADP \tab    up \tab H-90 \tab          -P            \tab -R \tab   X \tab  Y \tab  Z\cr
6 \tab Sontek \tab   ADP \tab  down \tab H-90 \tab          -P            \tab -R \tab   X \tab  Y \tab  Z\cr
} 

    Finally, a standardized rotation matrix is used to convert from ship
    coordinates to earth coordinates.  As described in the RDI coordinate
    transformation manual [1, pages 13 and 14], this matrix is based on sines
    and cosines of heading, pitch, and roll  If \code{CH} and \code{SH} denote
    cosine and sine of heading (after adjusting for declination), with similar
    terms for pitch and roll using second letters \code{P} and \code{R}, the
    rotation matrix is

    \preformatted{
rbind(c( CH*CR + SH*SP*SR,   SH*CP,    CH*SR - SH*SP*CR),
      c(-SH*CR + CH*SP*SR,   CH*CP,   -SH*SR - CH*SP*CR),
      c(           -CP*SR,      SP,               CP*CR))
    }

    This matrix is left-multiplied by a matrix with three rows, the top a
    vector of "starboard" values, the middle a vector of "forward" values, and
    the bottom a vector of "mast" values.  Finally, the columns of
    \code{data$ma$v[,,1:3]} are filled in with the result of the matrix
    multiplication.

}

\value{An object with \code{data$ma$v[,,1:3]} altered appropriately,
  and \code{metadata$oce.orientation} changed from \code{xyz} to
  \code{enu}.}

\references{
    \enumerate{
        \item RD Instruments, 1998.
        \emph{ADCP Coordinate Transformation, formulas and calculations.}
        P/N 951-6079-00 (July 1998).
        \item Clark Richards, 2011, PhD Dalhousie University Department of Oceanography.
        \item The SLEIWEX experiment (\url{http://myweb.dal.ca/kelley/SLEIWEX/index.php}).
    }
}

\seealso{See \code{\link{read.adp}} for other functions that relate to objects
of class \code{"adp"}.}

\author{Dan Kelley}

\keyword{misc}
