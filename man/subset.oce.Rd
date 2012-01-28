\name{subset.oce}

\alias{subset.oce}

\title{Subset an oce object}

\description{Subset an oce object}

\usage{\method{subset}{oce}(x, subset, indices=NULL, debug=getOption("oceDebug"), \dots)}

\arguments{
  \item{x}{an \code{oce} object.}
  \item{subset}{a condition to be applied to the \code{data} portion of
    \code{x}.  See \sQuote{Details}.}
  \item{indices}{list of station indices (used only for \code{section} objects).}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
    moderate amount of debugging information, or to 2 to get more.} 
  \item{\dots}{ignored.}
}

\details{This function is somewhat analogous to
    \code{\link{subset.data.frame}}, but it is more limited because it works by
    individualized inspection of \code{x} depending on its class.  Only one
    independent variable may be used in \code{subset} in any call to the
    function, which means that repeated calls will be necessary to, subset
    based on more than one independent variable (e.g. time and distance).
}

\value{A new \code{oce} object.}

\seealso{\code{\link{window.oce}} provides a simpler, but less powerful,
  way to subset data.}

\examples{
library(oce)
# CTD data
data(ctd)
plot(ctd)
plot(subset.oce(ctd, pressure>10))

# ADP example, subsetted by time and distance
data(adp)
adpNear <- subset.oce(adp, distance < 10)
# Note that the data may be accessed directly, as below
adpNearEarly <- subset.oce(adpNear, time < mean(adpNear@data$time))
plot(adpNearEarly)
}

\author{Dan Kelley}

\keyword{misc}
