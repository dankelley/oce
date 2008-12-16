\name{rbrtdr.trim}
\alias{rbrtdr.trim}
\title{Trim start/end portions of a RBRTDR cast}
\description{Trim start/end portions of a RBRTDR cast.}
\usage{rbrtdr.trim(x, method="water",parameters=NULL, verbose=FALSE)}
\arguments{
  \item{x}{A \code{rbrtdr} object, e.g. as read by \code{\link{read.rbrtdr}}.}
  \item{method}{Various methods exist, some of which use parameters:
    \item{"water"}{Trim data at start and end that seem to be in air (i.e. that have pressure very near 10db, i.e 100kPa.} 
    \item{"time"}{Select values only in indicated range of times,
      \emph{e.g.} \code{selection <- rbrtdr.trim(rbrtdr, "time", ...}
      selects ....}
    \item{"index"}{Select data only if the range of indices given in the two-element parameters list}
  }
  \item{parameters}{Depends on method; see above.}
  \item{verbose}{If set to TRUE, some debugging information is provided.}
}

\details{The \code{"water"} method is mainly for quick and dirty work.
In many cases, the user will be working with several files, and so it
will make sense to use the \code{"time"} method, to synchronize the
time series.  Normally, notes will have been taken in the field, so no
guessing need be done about the time the instruments went in the
water.  In some cases, though, the time will have to be inferred from
the data, and so it might make sense to start by trimming with the
\code{"water"} method, after which \code{\link{summary.rbrtdr}} is
used to find the probably time when the instrument was put into the
water.}

\value{An object of \code{\link[base]{class}} \code{"rbrtdr"}, with data having been trimmed in some way.}

\seealso{
  The \code{rbrtdr} object may be read with \code{\link{read.rbrtdr}}.
  \code{\link{plot.rbrtdr}} is very useful in providing guidance
  for trimming with \code{rbrtdr.trim}.
}
\examples{
\dontrun{
library(oce)
d <- read.rbrtdr("011855_1a.dat")
plot(d)                                 # contains initial/final junk
dt <- rbrtdr.trim(d, "time", c("2008-10-06 11:00:00","2008-10-09 10:00:00"))
plot(dt)                                # junk removed
}}

\author{Dan Kelley}
\keyword{misc}
