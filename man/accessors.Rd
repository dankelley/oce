\name{accessors}

\alias{heading}
\alias{heading<-}
\alias{pitch}
\alias{pitch<-}
\alias{roll}
\alias{roll<-}
\alias{temperature}
\alias{temperature<-}
\alias{salinity}
\alias{salinity<-}
\alias{pressure}
\alias{pressure<-}
\alias{latitude}
\alias{latitude<-}
\alias{longitude}
\alias{longitude<-}
\alias{time}
\alias{elevation}

\title{Access or modify part of an Oce object}

\description{Access or modify part of an Oce object}

\usage{
heading(x, time)
heading(x) <- value
pitch(x, time)
pitch(x) <- value
roll(x, time)
roll(x) <- value
salinity(x, time)
salinity(x) <- value
temperature(x, time)
temperature(x) <- value
pressure(x, time)
pressure(x) <- value
latitude(x, time)
latitude(x) <- value
longitude(x, time)
longitude(x) <- value
time(x, time)
elevation(x, time)
}

\arguments{

  \item{x}{an \code{oce} object.}

  \item{time}{optional vector of POSIX times, or object containing times in a
      field named \code{data$time} or \code{data$timeSlow}.  (If missing, 
      values at all the times in \code{x} are returned.)}

  \item{value}{value to assign to the relevant item in the \code{x} object.}

}

\details{These accessor functions provide a convenient way to discover, or set,
    data within \code{oce} objects.  This prevents the user from having to know
    the details of storage, e.g.  that Nortek Vector velocimeters record angles
    on slow timescales compared with velocities (stored in
    \code{x$data$timeSlow}, \code{x$data$headingSlow}, etc), whereas Sontek ADV
    velocimeters record them on the same timescale as velocity (stored in
    \code{x$data$time}, \code{x$data$heading}, etc.)}

\value{For the retrieval forms, the return value is value of the indicated
    portion of \code{x}; for the assignment forms, there is no return value.}

\examples{
library(oce)
data(adp)
print(heading(adp))
heading(adp) <- 5 + heading(adp) # add 5 degrees to the heading
print(heading(adp))
}

\seealso{Similar accessor functions are \code{\link{time}}, and \code{\link{velocity}}.}

\author{Dan Kelley}

\keyword{misc}
