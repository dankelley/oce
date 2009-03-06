\name{GMT.offset.from.tz}
\alias{GMT.offset.from.tz}
\title{Determine time offset from timezone}
\description{Determine time offset from timezone}
\usage{GMT.offset.from.tz(tz)}
\arguments{
\item{tz}{a timezone, e.g. \code{UTC}.}
}

\details{The data are from
  \url{http://www.timeanddate.com/library/abbreviations/timezones/} and
  were hand-edited to develop this code, so there may be errors.  Also,
  note that some of these contradict; if you examine the code, you'll
  see some commented-out portions that represent solving conflicting
  definitions by choosing the more common timezone abbreviation over a
  the less common one.}

\value{Number of hours in offset, e.g. \code{AST} yields 4.}

\examples{
library(oce)
cat("Atlantic Standard Time is ", GMT.offset.from.tz("AST"), "hours after UTC")
}
\author{Dan Kelley}
\keyword{misc}
