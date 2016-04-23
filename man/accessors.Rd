\name{accessors}
\alias{heading}
\alias{heading<-}
\alias{latitude}
\alias{latitude<-}
\alias{longitude}
\alias{longitude<-}
\alias{pitch}
\alias{pitch<-}
\alias{roll}
\alias{roll<-}

\title{Access or modify part of an Oce object}

\description{Access or modify part of an Oce object}

\usage{
heading(x, time)
heading(x) <- value
latitude(x, time, byDepth=TRUE)
latitude(x) <- value
longitude(x, time, byDepth=TRUE)
longitude(x) <- value
pitch(x, time)
pitch(x) <- value
roll(x, time)
roll(x) <- value
}

\arguments{

  \item{x}{an \code{oce} object.}

  \item{time}{optional vector of POSIX times, or object the \code{data} slot of
      which contains times in a field named \code{time} or \code{timeSlow}.
      (If this argument is not provided, missing, values at all the times in
      \code{x} are returned.)}

  \item{byDepth}{flag usd only for \code{"section"} objects, which indicates
      whether to repeat the \code{longitude} or \code{latitude} values so that
      there is a value for each depth in each profile.}

  \item{value}{value to assign to the relevant item in the \code{x} object.}

}

\details{Normally, users should access elements within \code{oce} objects using
    the double bracket notation, e.g. if \code{x} is an object of
    \code{\link{ctd-class}}, then \code{x[["salinity"]]} refers to the
    salinity, either as a value to be retrieved or one to be set.
    
    The accessor functions referred to above prove another way to access
    \code{oce} data. This is much more limited and are not generally
    recommended; they are included mainly so that old code will still work.

}

\value{Value of indicated portion of \code{x}.}

\examples{
library(oce)
data(adp)
print(heading(adp))
heading(adp) <- 5 + heading(adp) # add 5 degrees to the heading
print(heading(adp))
}

\seealso{Individual object classes handle accessing in specialized ways; see
    e.g. \code{\link{[[,ctd-method}} for how \code{[[} works for CTD objects.}

\author{Dan Kelley}

\keyword{misc}
