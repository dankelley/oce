\name{ctimeToSeconds}

\alias{ctimeToSeconds}

\title{Interpret a character string as a time interval}

\description{Interpret a character string as a time interval}

\usage{ctimeToSeconds(ctime)}

\arguments{
  \item{ctime}{a character string (see \sQuote{Details}.}
}

\details{Strings are of the form MM:SS or HH:MM:SS.}

\value{A numeric value, the number of seconds represented by the string.}

\examples{
library(oce)
cat("10      = ", ctimeToSeconds("10"), "s\n", sep="")
cat("01:04   = ", ctimeToSeconds("01:04"), "s\n", sep="")
cat("1:00:00 = ", ctimeToSeconds("1:00:00"), "s\n", sep="")
}

\seealso{See \code{\link{secondsToCtime}}, the inverse of this.}

\author{Dan Kelley}

\keyword{misc}
