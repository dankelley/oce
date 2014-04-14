\name{plot.landsat}

\alias{plot.landsat}
\alias{plot,landsat,missing-method}
\alias{plot,landsat-method}

\title{Plot landsat data}

\description{Plot landsat data.}

\usage{\S4method{plot}{landsat}(x, which=1, decimate=1, zlim, col=oceColorsJet,
    debug=getOption("oceDebug"), \dots)
}
\arguments{

  \item{x}{A \code{landsat} object, e.g. as read by
      \code{\link{read.landsat}}.}

  \item{which}{desired plot type; 1=histogram, 2=image}

  \item{zlim}{either a pair of numbers giving the limits for the colourscale,
      or \code{"histogram"} to have a flattened histogram (i.e. to maximally
      increase contrast throughout the domain.)}

  \item{col}{a function yielding colours, taking a single integer argument with
      the desired number of colours.}

  \item{decimate}{decimation interval for images plots; e.g. setting to 10
      means to show only every 10-th point in both directions, for a plot
      simplification by a factor 100.  The purpose is mainly to speed up
      plotting, which can be slow on large landsat images.}

  \item{debug}{set to a positive value to get debugging information during
      processing.}

  \item{\dots}{optional arguments passed to plotting functions.}

}

\details{This is a preliminary version of the function, and things are likely
    to change.  Additions will likely be motivated by research needs, e.g. it
    will probably not be a very high priority to produce natural-colour images
    since they are not as useful as false-colour for oceanographic use.}

\seealso{The documentation for \code{\link{landsat-class}} explains the
    structure of landsat objects, and also outlines the other functions dealing
    with them.  The \code{ocedata} package provides a dataset named
    \code{landsat}.}

\examples{
library(oce)
library(ocedata)
data(landsat)
plot(landsat, which=1, xlim=c(0.10, 0.13), breaks=200)
plot(landsat, which=2, zlim=c(0.10, 0.13))
}

\author{Dan Kelley}

\keyword{hplot}
