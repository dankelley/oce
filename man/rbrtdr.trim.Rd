\name{rbrtdr.trim}
\alias{rbrtdr.trim}
\title{Trim start/end portions of a RBRTDR cast}
\description{Trim start/end portions of a RBRTDR cast.}
\usage{rbrtdr.trim(x, method="median",parameters=NULL, verbose=FALSE)}
\arguments{
  \item{x}{A \code{rbrtdr} object, e.g. as read by \code{\link{read.rbrtdr}}.}
  \item{method}{Various methods exist, some of which use parameters:
    \item{"median"}{Select only data near the median pressure.}
    \item{"time"}{Select values only in indicated range of times,
      \emph{e.g.} \code{selection <- rbrtdr.trim(rbrtdr, "time", ...}
      selects ....}
    \item{"index"}{Select data only if the range of indices given in the two-element parameters list}
  }
  \item{parameters}{Depends on method; see above.}
  \item{verbose}{If set to TRUE, some debugging information is provided.}
}

\details{...}

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

\references{(fill in later)}
\author{Dan Kelley}
\keyword{misc}
