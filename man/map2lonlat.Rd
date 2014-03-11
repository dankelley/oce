\name{map2lonlat}

\alias{map2lonlat}

\title{Find lon-lat coordinates of a point on a map}

\description{Find lon-lat coordinates of a point on a map}

\usage{map2lonlat(xusr, yusr, tolerance=1e-4)}

\arguments{
    \item{xusr, yusr}{coordinates of points on the map.}
    \item{tolerance}{tolerance for misfit.}
}

\details{The location is inferred by trying to invert the results of the
    \code{\link[mapproj]{mapproject}} function.  The inversion is done by using
    \code{\link{optim}} to find the best match between a point in geographical
    space and plot space, and this match is not always good, being dependent on
    a starting guess.}

\value{A list containing \code{longitude} and \code{latitdue}.}

\seealso{See \code{\link{mapPlot}} for general information on plotting maps,
    including other functions.}

\author{Dan Kelley}

