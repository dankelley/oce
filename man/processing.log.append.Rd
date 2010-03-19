\name{processing.log.append}

\alias{processing.log.append}

\title{Append item to the processing log of object}

\description{Appends a time-stamped item to the processing log of an object.}

\usage{processing.log.append(x, action="")}

\arguments{
  \item{x}{an \code{oce} object.}
  \item{action}{a character string describing the action.}
}

\details{This is used to add a time-stamped item to the processing log
  of the object.}

\value{The object, with a new entry in its log.}

\examples{
\dontrun{
# This (and another typographical correction) was done in creating data(section).
data(ctd)
ctd$metadata$latitude <- ctd$metadata$latitude + 1
ctd <- processing.log.append(ctd, "DEK: move station a degree north")
}
}

\author{Dan Kelley}

\keyword{misc}
