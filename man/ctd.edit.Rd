\name{ctd.edit}
\alias{ctd.edit}
\title{Edit a CTD profile}
\description{
Edit an element of a CTD profile
}
\usage{ctd.edit(x, item, value,
         reason="not specified", person="not specified")}
\arguments{
\item{x}{a \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.}
\item{item}{a character string naming the item}
\item{value}{the new value for the item}
\item{reason}{the reason for the change}
\item{person}{name of person making the change}
}
}
\details{
As of now, only elements in the metadata can be altered.  Most commonly,
this will be latitude or longitude -- two elements that are quite
commonly entered incorrectly in data files.
} %details

\value{An object of \code{\link[base]{class}} \code{"ctd"}, with a new
  value for the item (and with a new line in the processing log).}

\seealso{
A \code{ctd} object may be read with \code{\link{read.ctd}},
and \code{\link{ctd.trim}} is useful in trimming spurious data
(e.g. those obtained during the upcast).
}
\examples{
library(oce)
data(ctd)
ctd2 <- ctd.edit(ctd, "latitude", 47.8879, "change last digit, for illustration", "Dan Kelley")
summary(ctd2)
}
\author{Dan Kelley}
\keyword{misc}
