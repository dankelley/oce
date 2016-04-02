\name{accessors}
\alias{[[,coastline,string,missing,ANY-method}
\alias{conductivity}
\alias{conductivity<-}
\alias{elevation}
\alias{distance}
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
\alias{oxygen}
\alias{oxygen<-}
\alias{nitrate}
\alias{nitrate<-}
\alias{nitrite}
\alias{nitrite<-}
\alias{phosphate}
\alias{phosphate<-}
\alias{silicate}
\alias{silicate<-}
\alias{spice}
\alias{tritium}
\alias{time}
\alias{velocity}

\title{Access or modify part of an Oce object}

\description{Access or modify part of an Oce object}

\usage{
conductivity(x) # whatever unit is in the object
conductivity(x) <- value # sets conductivityunit to "ratio"
elevation(x, time)
distance(x, time)
heading(x, time)
heading(x) <- value
latitude(x, time, byDepth=TRUE)
latitude(x) <- value
longitude(x, time, byDepth=TRUE)
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
oxygen(x, time)
oxygen(x) <- value
nitrate(x, time)
nitrate(x) <- value
nitrite(x, time)
nitrite(x) <- value
phosphate(x, time)
phosphate(x) <- value
silicate(x, time)
silicate(x) <- value
spice(x, time)
tritium(x, time)
time(x)
velocity(x)
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
