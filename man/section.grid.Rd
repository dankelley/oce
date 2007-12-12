\name{section.grid}
\alias{section.grid}
\title{Grid a section}
\description{Grid a section, by interpolating to fixed pressure levels.}
\usage{section.grid(section, p, method=c("approx","boxcar","lm"), ...)}
\arguments{
  \item{section}{A section to be gridded.}
  \item{p}{optional indication of the pressure levels to which interpolation should be done.  If this is not supplied, the pressure levels will be calculated based on the typical spacing in the ctd profiles stored within \code{section}.  If a single numerical value is provided, it is taken as a step for a seq() running from 0 to the maximum pressure (rounded to that step).  If \code{p="levitus"}, then the levels of the Levitus atlas are used.  If a list of numerical values is provided, then it is used as is.}
  \item{method}{the algorithm to use for interpolation to the pressure levels; see Details below for a sketch, and see \code{\link{ctd.decimate}} for a deeper discussion.}
  \item{...}{optional arguments to be supplied to \code{\link{ctd.decimate}}.}
}
\details{
The \code{"approx"}, \code{"boxcar"} and \code{"lm"} methods are described in the documentation for \code{\link{ctd.decimate}}, which is used to do this processing.  The default \code{"approx"} method is best for bottle data, the \code{"boxcar"} is best for ctd data, and the \code{"lm"} method is probably too slow to recommend for exploratory work, in which it is common to do trials with a variety of \code{"p"} values.
}
\value{
  A \code{section} object.  
}
\seealso{
Sections are normally created with \code{\link{read.section}} or \code{\link{make.section}}.
}
\examples{
# Gulf Stream
library(oce)
# File source -- http://cchdo.ucsd.edu/data_access?ExpoCode=90CT40_1
# a03 <- read.section("a03_hy1.csv")
data(a03)
a03g <- section.grid(a03, p=seq(0,2000,50))
data(coastline.world)
# Select the Gulf Stream region
plot(a03g,station.indices=seq(124,102),coastline=coastline.world,map.xlim=c(-80,-60))
}
\references{
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}
