\name{matlab2POSIXt}

\alias{matlab2POSIXt}

\title{Convert matlab time to POSIX time}

\description{Convert matlab time to POSIX time}

\usage{matlab2POSIXt(t, tz="UTC")}

\arguments{
  \item{t}{a Matlab \code{datenum} time, defined by the following
    sentence in the matlab documentation: \dQuote{where
      datenum('Jan-1-0000 00:00:00') returns the number 1}.}
  \item{tz}{the timezone.}
}

\value{A \code{\link{POSIXct}} time.}

\examples{
t <- 719529 # in matlab, datenum(1970, 1, 1, 0, 0, 0)
print(matlab2POSIXt(t))
}

\author{Dan Kelley}

\keyword{misc}
