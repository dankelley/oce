\name{processingLogItem}

\alias{processingLogItem}

\title{Create a processingLogItem}

\description{Create a processingLogItem..}

\usage{processingLogItem(action="")}

\arguments{
  \item{action}{A character string describing the action.}
}

\details{This is used, mainly by internal oce functions, to create a
  single-entry processing log.}

\value{An object of class \code{"processing.log"}, suitable for
  inclusion in any \code{"oce"} object.}

\seealso{\code{\link{processingLogAdd}}}

\author{Dan Kelley}

\keyword{misc}
