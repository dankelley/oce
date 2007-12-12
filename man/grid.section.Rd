\name{grid.section}
\alias{grid.section}
\title{Grid a section}
\description{Grid a section, by interpolating to fixed pressure levels.}
\usage{grid.section(section, pressures=NULL, method=c("approx","boxcar","lm"), ...)}
\arguments{
  \item{section}{A section to be gridded.}
  \item{pressures}{Pressure levels to which interpolation should be done.  If this is not supplied, the profiles will be examined and a vertical grid will be created that has approximately 100 levels at the deepest station.  If a single value is provided, it is taken as a step for a seq() running from 0 to the maximum pressure (rounded to the step).  If a list is given, it is used.  If \code{pressures} is set to the string \code{"levitus"}, then standard levels are used, as in the Levitus atlas.}
  \item{method}{the algorithm to use for interpolation, either the default \code{"approx"} to use the \code{\link{approx}} R function, or \code{"smooth.spline"} to use \code{\link{smooth.spline}} R function (see Details).}
  \item{...}{optional arguments to be supplied to the interpolation algorithm, e.g. \code{df} might be used with the \code{smooth.spline} method.}
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
a03g <- grid.section(a03, pressures=seq(0,2000,50))
data(coastline.world)
# Select the Gulf Stream region
plot(a03g,station.indices=seq(124,102),coastline=coastline.world,map.xlim=c(-80,-60))
}
\references{
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}
