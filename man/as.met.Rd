\name{as.met}
\alias{as.met}
\title{Coerce data into met dataset}
\description{Coerces a dataset into a met dataset.}

\usage{
as.met(time, temperature, pressure, u, v, filename="(constructed from data)")
}
\arguments{

  \item{time}{Vector of obseration times (or character strings that can be
      coerced into times).}

  \item{temperature}{vector of temperatures.}

  \item{pressure}{vector of pressures.}

  \item{u}{vector of eastward wind speed in m/s.}

  \item{v}{vector of northward wind speed in m/s.}

  \item{filename}{optional string indicating data source}
}

\details{This function is used by \code{\link{read.met}}, and may be used to
    construct objects that behave as though read by that function.}

\value{An object of \code{\link[base]{class}} \code{"met"} (for details, see
    \code{\link{met-class}}).}

\seealso{The documentation for \code{\link{met-class}} explains the structure
    of \code{met} objects, and also outlines the other functions dealing with
    them.}

\author{Dan Kelley}

\keyword{misc}
