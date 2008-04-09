\name{oce.edit}
\alias{oce.edit}
\title{Edit an oce object}
\description{Edit an element of a oce object}
\usage{oce.edit(x, item, value,
         reason="not specified", person="not specified")}
\arguments{
\item{x}{a \code{oce} object.}
\item{item}{a character string naming an item in the object's metadata}
\item{value}{the new value for the item}
\item{reason}{the reason for the change}
\item{person}{name of person making the change}
}

\details{As of now, only elements in the metadata can be altered.}

\value{An object of \code{\link[base]{class}} \code{"oce"}, with a new
  value for the item (and with a new line in the processing log).}

\examples{
library(oce)
data(ctd)
ctd2 <- oce.edit(ctd, "latitude", 47.8879, "change last digit, for illustration", "Dan Kelley")
summary(ctd2)
}
\author{Dan Kelley}
\keyword{misc}
