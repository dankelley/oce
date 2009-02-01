\name{oce.edit}
\alias{oce.edit}
\title{Edit an oce object}
\description{Edit an element of a oce object}
\usage{oce.edit(x, item, value, action,
         reason="not specified", person="not specified")}
\arguments{
\item{x}{a \code{oce} object.}
\item{item}{if supplied, a character string naming an item in the
  object's metadata (see Details)}
\item{value}{new value for item, if both supplied}
\item{action}{action to be carried out on the object (see Details)}
\item{reason}{the reason for the change}
\item{person}{name of person making the change}
}

\details{If both an \code{item} and \code{value} are supplied, then the
  object's metadata are examined to see if the \code{item} exists
  there.  If so, it is updated to the supplied value.

  If \code{item} and \code{value} are not supplied, then \code{action}
  must be supplied.  This is a character string specifying some action
  to be performed on the object, e.g. a manipulation of a column.  The
  action must refer to the object as \code{x}; see Examples.
}

\value{An object of \code{\link[base]{class}} \code{"oce"}, with a new
  value for the item (and with a new line in the processing log).}

\examples{
library(oce)
data(ctd)
ctd2 <- oce.edit(ctd, item="latitude", value=47.8879, reason="illustration", person="Dan Kelley")
ctd3 <- oce.edit(ctd, action= "x$data$pressure <- x$data$pressure - 1")
}
\author{Dan Kelley}
\keyword{misc}
