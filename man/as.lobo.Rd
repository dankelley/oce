\name{as.lobo}

\alias{as.lobo}

\title{Coerce data into lobo dataset}

\description{Coerce a dataset into a lobo dataset.}

\usage{
as.lobo(time, u, v, salinity, temperature, pressure, nitrate, fluorescence, filename="")
}

\arguments{
  \item{time}{vector of times of observation}
  \item{u}{vector of x velocity component observations}
  \item{v}{vector of y velocity component observations}
  \item{salinity}{vector of salinity observations}
  \item{temperature}{vector of temperature observations}
  \item{pressure}{vector of pressure observationss}
  \item{nitrate}{vector of nitrate observationss}
  \item{fluorescence}{vector of fluoresence observations}
  \item{filename}{source filename}
}

\details{This function assembles vectors into a \code{lobo} object.}

\value{An object of \code{\link[base]{class}} \code{"lobo"}.}

\seealso{The documentation for \code{\link{lobo-class}} explains the
    structure of lobo objects, and also outlines the other functions dealing
    with them.}

\author{Dan Kelley}

\keyword{misc}
