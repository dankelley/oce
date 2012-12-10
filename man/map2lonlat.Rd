\name{map2lonlat}

\alias{map2lonlat}

\title{Find lon-lat coordinator of a point on a map}

\description{Find lon-lat coordinator of a point on a map}

\usage{map2lonlat(xusr, yusr, tolerance=1e-5)}

\arguments{
    \item{xusr, yusr}{coordinates of points on the map.}
    \item{tolerance}{tolerance for misfit.}
}

\details{Uses \code{\link{optim}} to find the location, so it may fail in some circumstances.}

\value{A list containing \code{longitude} and \code{latitdue}.}

\seealso{See \code{\link{mapPlot}} for general information on plotting maps,
    including other functions.}

\author{Dan Kelley}

