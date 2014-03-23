% vim:textwidth=100:expandtab:shiftwidth=2:softtabstop=2
\name{landsatTrim}

\alias{landsatTrim}

\title{Trim a landsat image to a lat-lon box}

\description{Trim a landsat image to a lat-lon box.}

\usage{landsatTrim(x, ll, ur)}

\arguments{
  \item{x}{A \code{landsat} object, e.g. as read by \code{\link{read.landsat}}.}

  \item{ll}{A list containing \code{longitude} and \code{latitude}, for the lower-left corner of the
    portion of the image to retain.}

  \item{ur}{A list containing \code{longitude} and \code{latitude}, for the upper-right corner of
    the portion of the image to retain.}

}

\details{An error results if there is no intersection between the trimming box and the image box.
  Note that since the image is not bounded by a rectangular box, this method is only approximate.}
  
\value{An object of \code{\link[base]{class}} \code{"landsat"}, with data having been trimmed in some way.}

\seealso{The documentation for \code{\link{landsat-class}} explains the structure
    of landsat objects, and also outlines the other functions dealing with them.}

\author{Dan Kelley}

\keyword{misc}

