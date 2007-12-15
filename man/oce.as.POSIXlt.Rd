\name{oce.as.POSIXlt}
\alias{oce.as.POSIXlt}
\title{More general form of as.POSIXlt}
\description{
A more general form of as.POSIXlt.
}
\usage{
oce.as.POSIXlt(x, tz = "")
}
\arguments{
  \item{x}{a date, as for \code{as.POSIXlt}, but also including forms in which the month name appears.}
  \item{tz}{the timezone, as for \code{as.POSIXlt}}
}
\details{
Used in parsing headers, this function is built on the standard \code{\link{as.POSIXlt}} function.
the only difference is that this also recognizes dates of forms such as
\code{"Aug 23 2002"},
\code{"August 23 2002"},
\code{"2002 Aug 23"}, and
\code{"2002 23 Aug"}.  (The month may appear in abbreviated form or written in full,
and may be capitalized or not.)
}
\value{
A POSIXlt object.
}
\seealso{
\code{as.POSIXlt}, from which this is derived.
}
\examples{
}
\author{Dan Kelley}
\keyword{misc}
