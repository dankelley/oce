\name{xyz.to.enu.adv}

\alias{xyz.to.enu.adv}

\title{Convert ADV from xyz coordinates to enu coordinates}

\description{Convert ADV velocity components from a xyz-based
  coordinate system to an enu-based coordinate system.}

\usage{xyz.to.enu.adv(x, declination=0,
               cabled=FALSE, horizontal.case, sensor.orientation,
               debug=getOption("oce.debug"))}

\arguments{
  \item{x}{an object of class \code{"adv"}.}
  \item{declination}{magnetic declination to be added to the heading, to
    get ENU with N as "true" north.}

  \item{cabled}{boolean value indicating whether the sensor head is connected
      to the pressure case with a cable.  If \code{cabled=FALSE}, then
      \code{horizontal.case} is ignored.  See \dQuote{Details}.}

  \item{horizontal.case}{optional boolean value indicating whether the sensor
      case is oriented horizontally.  Ignored unless \code{cabled} is
      \code{TRUE}. See \dQuote{Details}.}

  \item{sensor.orientation}{optional string indicating the direction in which
      the sensor points.  The value, which must be \code{"upward"} or
      \code{"downward"}, over-rides the value of \code{x$metadata$orientation},
      which will have been set by \code{\link{read.adv}}, \emph{provided} that
      the data file contained the full header.  See \dQuote{Details}.}

  \item{debug}{a flag that, if non-zero, turns on debugging.  Higher values
      yield more extensive debugging.}

}

\details{The coordinate transformation is done using the heading, pitch,
  and roll information contained within \code{x}.  The algorithm is
  similar to that used for Teledyne/RDI ADCP units, taking into account
  the different definitions of heading in the two cases.

  Generally, the transformation must be done on a time-by-time basis,
  which is a slow operation.  However, this function checks whether the
  vectors for heading, pitch and roll, are all of unit length, and in
  that case, the calculation time is reduced by an order of magnitude.
  This situation of singleton vectors is not uncommon, because the
  orientation data can be noisy, requiring the user to substitute
  cleaned-up values, and if the instrument is moored on a steady
  platform, it makes sense to supply single values.  Note that the
  angles are held in \code{data$ts.slow} for Nortek instruments and
  \code{data$ts} for Sontek instruments.

  To indicate the change in coordinate, the value of
  \code{metadata$oce.coordinate} is changed from \code{xyz} to \code{enu}, and
  this is noticed by e.g. \code{\link{plot.adv}}.

  Users are cautioned that this step in processing is tricky, because it is
  difficult to get clear indications on processing steps from the documents
  that manufacturers provide.  If results seem incorrect (e.g. if currents go
  east instead of west), users should examine the code in detail for the case
  at hand.  The first step is to set \code{debug} to 1, so that the processing
  will print a trail of processing steps.  The next step should be to consult
  the table below, to see if it matches the instrument and its configuration.

  This table results from examination of documentation provided by
  manufacturers, and of discussion on user groups.  This analysis was done by
  Clark Richards, a PhD student at Dalhousie University [reference 2].  The
  results are probably most correct for the instruments dealth with in his
  thesis.

  The column labelled ``Cabled'' indicates whether the sensor and the pressure
  case are connected with a flexible cable, and the column labelled ``H.case''
  indicates whether the pressure case is oriented horizontally.  These two
  properties are not discoverable in the headers of the data files, and so they
  must be supplied with the arguments \code{cabled} and \code{horizontal.case}.
  The source code refers to the information in this table by case numbers.
  (Cases 5 and 6 are not handled.)  Angle abbreviations: heading ``H,'' pitch
  ``P,'' and roll ``R''.  Instrument coordinates: x ``X,'' y ``Y,'', and z
  ``Z.'' Ship coordinates: starboard ``S,'' forward ``F,'' and mast ``M.''

\tabular{rrrrrrrrrrrr}{
\strong{Case} \tab \strong{Mfr.} \tab \strong{Instr.} \tab \strong{Cabled} \tab \strong{H. case} \tab \strong{Orient.} \tab \strong{H}  \tab \strong{P} \tab \strong{R} \tab \strong{S} \tab \strong{F} \tab \strong{M}\cr 
1 \tab Nortek \tab vector \tab    no \tab      - \tab     up  \tab H-90 \tab  R \tab -P \tab  X \tab -Y \tab -Z\cr
2 \tab Nortek \tab vector \tab    no \tab      - \tab   down  \tab H-90 \tab  R \tab -P \tab  X \tab  Y \tab  Z\cr
3 \tab Nortek \tab vector \tab   yes \tab    yes \tab     up  \tab H-90 \tab  R \tab -P \tab  X \tab  Y \tab  Z\cr
4 \tab Nortek \tab vector \tab   yes \tab    yes \tab   down  \tab H-90 \tab  R \tab  P \tab  X \tab -Y \tab -Z\cr
5 \tab Nortek \tab vector \tab   yes \tab     no \tab     up  \tab  -   \tab  - \tab  - \tab  - \tab  - \tab  -\cr
6 \tab Nortek \tab vector \tab   yes \tab     no \tab   down  \tab  -   \tab  - \tab  - \tab  - \tab  - \tab  -\cr
7 \tab Sontek \tab   adv  \tab    -  \tab      - \tab     up  \tab H-90 \tab  R \tab -P \tab  X \tab -Y \tab -Z\cr
8 \tab Sontek \tab   adv  \tab    -  \tab      - \tab   down  \tab H-90 \tab  R \tab -P \tab  X \tab  Y \tab  Z\cr
}
}

\seealso{See \code{\link{read.adv}} for notes on functions relating to
  \code{"adv"} objects.}

\author{Dan Kelley, in collaboration with Clark Richards}

\references{
    \enumerate{
\item \url{http://www.nortek-as.com/lib/forum-attachments/coordinate-transformation}
\item Clark Richards, 2010, PhD Dalhousie University Department of Oceanography.
    }
}


\keyword{misc}
