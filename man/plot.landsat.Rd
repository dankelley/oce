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

\details{This is a very preliminary version of the function, and things are
    likely to change.  In particular, since early tests with
    \code{\link{read.landsat}} have only been with a single band at a time,
    there is no facility to plot natural-colour, etc.}

\seealso{The documentation for \code{\link{landsat-class}} explains the
    structure of landsat objects, and also outlines the other functions dealing
    with them.}

\author{Dan Kelley}

\keyword{hplot}
