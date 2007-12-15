\name{as.ctd}
\alias{as.ctd}
\title{Coerce data into ctd dataset}
\description{
  Coerces a dataset into a ctd dataset.
}
\usage{
thectd <- as.ctd(
S, T, p, header=NULL, filename=NA, ship=NA, scientist=NA,
institute=NA, address=NA, cruise=NA, station=NA,
date=NA, start.time=NA, 
latitude=NA, longitude=NA,
recovery=NA,
water.depth=NA,
sample.interval=NA)
}
\arguments{
  \item{S}{Salinity through the water column.}
  \item{T}{Temperature through the water column.}
  \item{p}{pressure through the water column.}
  \item{header}{ignored, for now}
  \item{filename}{ignored, for now}
  \item{ship}{ignored, for now}
  \item{scientist}{ignored, for now}
  \item{institute}{ignored, for now}
  \item{address}{ignored, for now}
  \item{cruise}{ignored, for now}
  \item{station}{ignored, for now}
  \item{date}{ignored, for now}
  \item{start.time}{ignored, for now}
  \item{latitude}{ignored, for now}
  \item{longitude}{ignored, for now}
  \item{recovery}{ignored, for now}
  \item{water.depth}{ignored, for now}
  \item{sample.interval}{ignored, for now}
}
\details{
 This is helpful if data were created, not read in from a file.
}
\value{
  A \code{ctd} object.  
}
\seealso{
  \code{\link{read.ctd}} reads data,
}
\examples{
library(oce)
p <- seq(0,100,1)
T <- 10 - p / 100
S <- 35 - p / 100
ctd <- as.ctd(S, T, p)
summary(ctd)
}
\author{Dan Kelley}
\keyword{misc}
