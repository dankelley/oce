\name{oce.edit}
\alias{oce.edit}
\title{Edit an oce object}
\description{Edit an element of a oce object}
\usage{oce.edit(x, item, value, action,
         reason="not specified", person="not specified")}
\arguments{
\item{x}{a \code{oce} object.}
\item{item}{if supplied, a character string naming an item in the
  object's metadata (see \dQuote{Details}).}
\item{value}{new value for item, if both supplied.}
\item{action}{optional character string containing R code to carry
  out some action on the object.}
\item{reason}{character string giving the reason for the change.}
\item{person}{character string giving the name of person making the change.}
}

\details{There are two ways to use this function.

  1. If both an \code{item} and \code{value} are supplied, then the
  object's metadata entry named \code{item} is updated to the
  supplied \code{value}.
  
  2. If \code{item} and \code{value} are not supplied, then
  \code{action} must be supplied.  This is a character string specifying
  some action to be performed on the object, e.g. a manipulation of a
  column.  The action must refer to the object as \code{x}; see
  Examples.

  In each case, a log entry is stored in the object, to document the
  change.  Indeed, this is the main benefit to using this function,
  instead of altering the object directly.  The log entry will be most
  useful if it contains a brief note on the \code{reason} for the
  change, and the name of the \code{person} doing the work.}

\value{An object of \code{\link[base]{class}} \code{"oce"}, altered
  appropriately, and with a log item indicating the nature of the
  alteration.}

\examples{
library(oce)
data(ctd)
ctd2 <- oce.edit(ctd, item="latitude", value=47.8879,
                 reason="illustration", person="Dan Kelley")
ctd3 <- oce.edit(ctd, action= "x$data$pressure <- x$data$pressure - 1")
}
\author{Dan Kelley}
\keyword{misc}
