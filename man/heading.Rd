\name{heading}

\alias{heading}
\alias{pitch}
\alias{roll}
\alias{heading<-}
\alias{pitch<-}
\alias{roll<-}

\title{Return the header for an Oce data object}

\description{Return the header for an oce data  object.}

\usage{
heading(x, time)
pitch(x, time)
roll(x, time)
heading(x) <- value
pitch(x) <- value
roll(x) <- value
}

\arguments{

  \item{x}{an \code{oce} object that contains information on instrument
      heading, pitch, or roll.}

  \item{time}{optional vector of POSIX times, or object containing times in a
      field named \code{data$time} or \code{data$timeSlow}.  (If missing, all
      heading values are returned.)}

  \item{value}{value to assign to the relevant item in the \code{x} object.}

}

\details{These accessor functions provide a convenient way to discover, or set,
    the angles recorded by various instruments.  This prevents the user from
    having to know the details of storage, e.g. that Nortek Vector velocimeters
    record angles on slow timescales compared with velocities (stored in
    \code{x$data$timeSlow}, \code{x$data$headingSlow}, etc), whereas Sontek ADV
    velocimeters record them on the same timescale as velocity (stored in
    \code{x$data$time}, \code{x$data$heading}, etc.)}

\value{For the retrieval forms, the return value is a vector of angles;
    otherwise, it is the altered object (normally not stored).}

\examples{
library(oce)
data(adp)
print(heading(adp))
heading(adp) <- 5 + heading(adp) # add 5 degrees to the heading
print(heading(adp))
}

\author{Dan Kelley}

\keyword{misc}
