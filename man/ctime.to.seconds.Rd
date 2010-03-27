\name{ctime.to.seconds}

\alias{ctime.to.seconds}

\title{Interpret a character string as a time interval}

\description{Interpret a character string as a time interval}

\usage{ctime.to.seconds(time)}

\arguments{
  \item{time}{a character string (see \sQuote{Details}.}
}

\details{Strings are }

\value{A numeric value, the number of seconds represented by the string.}

\examples{
library(oce)
cat("Ten seconds = ", ctime.to.seconds("10"), "s\n", sep="")
cat("Sixty-four seconds = ", ctime.to.seconds("01:04"), "s\n", sep="")
cat("Thirty-six hundred seconds = ", ctime.to.seconds("1:00:00"), "s\n", sep="")
}

\author{Dan Kelley}

\keyword{misc}
