\name{formatPosition}

\alias{formatPosition}

\title{Geographical position in degrees and minutes}

\description{Format geographical positions to degrees, minutes, and hemispheres}

\usage{formatPosition(latlon, isLat=TRUE, type=c("list", "string", "expression"), showHemi=FALSE)}

\arguments{
  \item{latlon}{a vector of latitudes or longitudes}
  \item{isLat}{a boolean that indicates whether the quantity is latitude or longitude}
  \item{type}{a string indicating the type of return value (see below)}
  \item{showHemi}{a boolean that indicates whether to indicate the hemisphere}
}

\details{This function is in an early stage of development.}

\value{A list containing \code{degrees}, \code{minutes}, \code{seconds}, and
    \code{hemispheres}, or a vector of strings or (broken) a vector of
    expressions.}

\examples{
library(oce)
formatPosition(10+1:10/60+2.8/3600)
formatPosition(10+1:10/60+2.8/3600, type="string")
}

\author{Dan Kelley}

\keyword{misc}
