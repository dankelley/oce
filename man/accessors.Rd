\name{accessors}

\alias{elevation}
\alias{heading}
\alias{heading<-}
\alias{latitude}
\alias{latitude<-}
\alias{longitude}
\alias{longitude<-}
\alias{pitch}
\alias{pitch<-}
\alias{pressure}
\alias{pressure<-}
\alias{roll}
\alias{roll<-}
\alias{salinity}
\alias{salinity<-}
\alias{sigmaTheta}
\alias{sigmaTheta<-}
\alias{temperature}
\alias{temperature<-}
\alias{time}
\alias{velocity}

\title{Access or modify part of an Oce object}

\description{Access or modify part of an Oce object}

\usage{
elevation(x, time)
heading(x, time)
heading(x) <- value
latitude(x, time)
latitude(x) <- value
longitude(x, time)
longitude(x) <- value
pitch(x, time)
pitch(x) <- value
pressure(x, time)
pressure(x) <- value
roll(x, time)
roll(x) <- value
salinity(x, time)
salinity(x) <- value
sigmaTheta(x, time)
sigmaTheta(x) <- value
temperature(x, time)
temperature(x) <- value
time(x)
velocity(x)
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

\value{Value of indicated portion of \code{x}.}

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
