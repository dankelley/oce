\name{pt.trim}
\alias{pt.trim}
\title{Trim start/end portions of a pT cast}
\description{Trim start/end portions of a pT cast.}
\usage{pt.trim(x, method="water", parameters=NULL, debug=getOption("oce.debug"))}
\arguments{
  \item{x}{A \code{pt} object, e.g. as read by \code{\link{read.pt}}.}
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

\details{The \code{"water"} method is mainly for quick and dirty work.
  In many cases, the user will be working with several files, and so it
  will make sense to use the \code{"time"} method, to synchronize the
  time series.  Normally, notes will have been taken in the field, so no
  guessing need be done about the time the instruments went in the
  water.  In some cases, though, the time will have to be inferred from
  the data, and so it might make sense to start by trimming with the
  \code{"water"} method, after which \code{\link{summary.pt}} is used
  to find the probably time when the instrument was put into the water.

  After the data are trimmed in time, the pressure record is modified by
  subtracting 10.1325~dbar, the average sea-level pressure.

}

\value{An object of \code{\link[base]{class}} \code{"pt"}, with data
  having been trimmed in some way, and with the pressure being reduced
  by mean sea-level pressure.} 

\seealso{
  The \code{pt} object may be read with \code{\link{read.pt}}.
  \code{\link{plot.pt}} is very useful in providing guidance
  for trimming with \code{pt.trim}.
}
\examples{
library(oce)
data(pt)
plot(pt)
pt.trimmed <- pt.trim(pt)
plot(pt.trimmed)
}

\author{Dan Kelley}
\keyword{misc}
