\name{processing.log.add}

\alias{processing.log.add}

\title{Add an item to a processing log}

\description{Add an item to a processing log}

\usage{processing.log.add(pl, action="")}

\arguments{
  \item{pl}{a processing log.  If \code{NULL}, a new processing log is
    created.}
  \item{action}{a character string describing the action.}
}

\details{This is used to add a time-stamped item to the processing log
  of the object.}

\value{The processing log.}

\examples{
data(ctd)
ctd$metadata$latitude <- ctd$metadata$latitude + 1
ctd$processing.log <- processing.log.add(ctd$processing.log, "correct latitude")
}

\author{Dan Kelley}

\keyword{misc}
