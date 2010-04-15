\name{ctime.to.seconds}

\alias{ctime.to.seconds}

\title{Interpret a character string as a time interval}

\description{Interpret a character string as a time interval}

\usage{ctime.to.seconds(ctime)}

\arguments{
  \item{ctime}{a character string (see \sQuote{Details}.}
}

\details{Strings are of the form MM:SS or HH:MM:SS.}

\value{A numeric value, the number of seconds represented by the string.}

\examples{
library(oce)
cat("10      = ", ctime.to.seconds("10"), "s\n", sep="")
cat("01:04   = ", ctime.to.seconds("01:04"), "s\n", sep="")
cat("1:00:00 = ", ctime.to.seconds("1:00:00"), "s\n", sep="")
}

\seealso{See \code{\link{seconds.to.ctime}}, the inverse of this.}

\author{Dan Kelley}

\keyword{misc}
