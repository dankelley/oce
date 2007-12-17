\name{log.append}
\alias{log.append}
\title{Append item to the processing log of object}
\description{
Appends a time-stamped item to the processing log of an object.
}
\usage{log.append(x, action="")}
\arguments{
  \item{x}{An \code{oce} object.}
  \item{action}{A character string describing the action.}
}
\details{
This is used to add a time-stamped item to the processing log of the object.
}
\value{
The object, with a new entry in its log.
}
\seealso{
}
\examples{
\dontrun{
# This (and another typographical correction) was done in creating data(section).
stn15 <- ctd.decimate(ctd.trim(read.ctd("BED0315.CNV")),p=p)
stn15$latitude <- stn15$latitude + 1
stn15 <- log.append(stn15, "DEK: add 1 to latitude, correcting a typo")
}
}
\author{Dan Kelley}
\keyword{misc}
