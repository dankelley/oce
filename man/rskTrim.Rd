% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2

\name{rskTrim}
\alias{rskTrim}
\title{Trim start/end portions of a rsk cast}
\description{Trim start/end portions of a rsk cast.}

\usage{rskTrim(x, method="water", parameters=NULL, debug=getOption("oceDebug"))}

\arguments{

  \item{x}{A \code{rsk} object, e.g. as read by \code{\link{read.rsk}}.}

  \item{method}{Various methods exist, some of which use parameters:
    \describe{
      \item{\code{"water"}}{Trim data at start and end that seem to be in air
	(i.e. that have pressure very near 10db, i.e 100kPa.}
      \item{\code{"time"}}{Select values only in indicated range of times,
	provided in POSIX format.}
      \item{\code{"index"}}{Select data only if the range of indices given in
	the two-element parameters list}
    }
  }
  \item{parameters}{Depends on method; see above.}
  \item{debug}{a flag that turns on debugging.  The value indicates the
    depth within the call stack to which debugging applies.  For
    example, \code{read.adv.nortek()} calls \code{read.header.nortek()},
    so that \code{read.adv.nortek(...,debug=2)} provides information
    about not just the main body of the data file, but also the details
    of the header.}
}

\details{The \code{"water"} method is mainly for quick and dirty work.  In many
  cases, the user will be working with several files, and so it will make sense
  to use the \code{"time"} method, to synchronize the time series.  Normally,
  notes will have been taken in the field, so no guessing need be done about the
  time the instruments went in the water.  In some cases, though, the time will
  have to be inferred from the data, and so it might make sense to start by
  trimming with the \code{"water"} method, after which \code{\link{summary.rsk}}
  is used to find the probably time when the instrument was put into the water.

  After the data are trimmed in time, the pressure record is modified by
  subtracting 10.1325~dbar, the average sea-level pressure.

}

\value{An object of \code{\link[base]{class}} \code{"rsk"}, with data having
  been trimmed in some way, and with the pressure being reduced by mean
  sea-level pressure.} 

\seealso{The documentation for \code{\link{rsk-class}} explains the structure of
  \code{rsk} objects, and also outlines the other functions dealing with them.}

\examples{
library(oce)
data(rsk)
rskTrimmed <- rskTrim(rsk)
plot(rskTrimmed)
}

\author{Dan Kelley}
\keyword{misc}
