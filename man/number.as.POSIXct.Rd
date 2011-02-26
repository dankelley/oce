\name{number.as.POSIXct}

\alias{number.as.POSIXct}

\title{Convert an integer time to a POSIXct time}

\description{Convert an integer time to a POSIXct time}

\usage{number.as.POSIXct(t, type=c("unix"), tz="UTC")}

\arguments{

    \item{t}{an integer corresponding to a time (see \dQuote{Details}).}

    \item{type}{the type of time (see \dQuote{Details}).}

    \item{tz}{a string indicating the time zone.}

}

\details{Only one \code{type} is permitted in the present version of the
    function, namely \code{"unix"}, and the definition of \code{t} in that case
    is the number of seconds since the start of January 1, 1970 (i.e. since
    midnight on the last day of 1969).
}

\value{A POSIXct time.}

\examples{
cat("Unix times start at ", number.as.POSIXct(0), "\n")
}

\author{Dan Kelley}

\keyword{misc}
