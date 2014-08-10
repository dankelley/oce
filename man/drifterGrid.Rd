\name{drifterGrid}

\alias{drifterGrid}

\title{Grid a drifter}

\description{Grid a drifter, by interpolating to fixed pressure levels.}

\usage{drifterGrid(drifter, p, debug=getOption("oceDebug"), \dots)}

\arguments{

  \item{drifter}{a \code{drifter} object to be gridded.}

  \item{p}{optional indication of the pressure levels to which interpolation
      should be done.  If this is not supplied, the pressure levels will be
      calculated based on the existing values, using medians. If
      \code{p="levitus"}, then pressures will be set to be those of the Levitus
      atlas, given by \code{\link{standardDepths}}, trimmed to the maximum
      pressure in \code{drifter}.  If \code{p} is a single numerical value, it
      is taken as the number of subdivisions to use in a call to
      \code{\link{seq}} that has range from 0 to the maximum pressure in
      \code{drifter}.  Finally, if a vector numerical values is provided, then
      it is used as is.}

  \item{debug}{a flag that turns on debugging.  Higher values provide deeper
      debugging.}
  
  \item{\dots}{optional arguments to \code{\link{approx}}, which is used
      to do the gridding.}

}

\details{The gridding is done with \code{\link{approx}}.  If there is
    sufficient user demand, other methods may be added, by analogy to
    \code{\link{sectionGrid}}.}

\value{An object of \code{\link[base]{class}} \code{"drifter"} that contains a
    pressure matrix with constant values along the first index.}

\seealso{The documentation for \code{\link{drifter-class}} explains the
    structure of drifter objects, and also outlines the other functions dealing
    with them.}

\examples{
library(oce)
data(drifter)
g <- drifterGrid(drifter, p=seq(0, 100, 1))
par(mfrow=c(2,1))
t <- g[["time"]]
z <- -g[["pressure"]][,1]
## Set zlim because of spurious temperatures.
imagep(t, z, t(g[['temperature']]), ylim=c(-100,0), zlim=c(0,20))
imagep(t, z, t(g[['salinity']]), ylim=c(-100,0))
}

\author{Dan Kelley}

\keyword{misc}
