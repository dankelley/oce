\name{as.echosounder}
\alias{as.echosounder}
\title{Coerce data into echosounder dataset}
\description{Coerces a dataset into a echosounder dataset.}

\usage{
as.echosounder(time, depth, amplitude, src="")
}
\arguments{
  \item{time}{times of pings}
  \item{depth}{depths of samples within pings}
  \item{amplitude}{matrix of amplitudes}
  \item{src}{optional string indicating data source}
}

\details{Creates a biosonics echosounder file.}

\value{An object of \code{\link[base]{class}} \code{"echosounder"}; for details
    of this data type, see \code{\link{echosounder-class}}).}

\seealso{The documentation for \code{\link{echosounder-class}} explains the
    structure of \code{echosounder} objects, and also outlines the other
    functions dealing with them.}

\author{Dan Kelley}

\keyword{misc}
