\name{heading}

\alias{heading}
\alias{pitch}
\alias{roll}

\title{Return the header for an Oce data object}

\description{Return the header for an oce data  object.}

\usage{
heading(x, time)
pitch(x, time)
roll(x, time)
}

\arguments{

  \item{x}{an \code{oce} object that contains information on instrument
      heading, pitch, or roll.}

  \item{time}{optional vector of POSIX times; if missing, all heading values
      are returned.}

}

\details{Returns the indicated angle, interpolated to the indicated times with
    \code{\link{approx}}.  If \code{time} is not given, the entire stored vector
    for the angle is returned}

\value{A vector of angles.}

\examples{
library(oce)
data(adp)
heading(adp)
}

\author{Dan Kelley}

\keyword{misc}
