\name{processing.log.item}

\alias{processing.log.item}

\title{Create a processing-log item}

\description{Create a processing-log item..}

\usage{processing.log.item(log.action="")}

\arguments{
  \item{log.action}{A character string describing the action.}
}

\details{This is used, mainly by internal oce functions, to create a
  single-entry processing log.}

\value{An object of class \code{"processing.log"}, suitable for
  inclusion in any \code{"oce"} object.}

\seealso{\code{\link{processing.log.append}}}

\author{Dan Kelley}

\keyword{misc}
