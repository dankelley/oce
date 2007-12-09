\name{grid.section}
\alias{grid.section}
\title{Grid a section}
\description{Grid a section, by interpolating to fixed pressure levels.}
\usage{grid.section(section, pressures=NULL, algorithm=c("smooth.spline","approx"), ...)}
\arguments{
  \item{section}{A section to be gridded.}
  \item{pressures}{Pressure levels to which interpolation should be done.  If this is not supplied, the profiles will be examined and a vertical grid will be created that has approximately 100 levels at the deepest station.  If a single value is provided, it is taken as a step for a seq() running from 0 to the maximum pressure (rounded to the step).  If a list is given, it is used.  If \code{pressures} is set to the string \code{"levitus"}, then standard levels are used, as in the Levitus atlas.}
  \item{algorithm}{algorithm to use for interpolation.  The default \code{\link{smooth.spline}} method employs a smoothing spline, which generally runs very close to the data and has the advantage of producing smoothly varying results.  The \code{\link{approx}} is a linear interpolation between adjacent points, which has the advantage of being predictable, but it has two disadvantages: (1) the resultant profiles will have discontinous second derivatives, and (2) the algorithm uses only nearest-neighbours, and thus it discards data.}
  \item{...}{optional arguments to be supplied to the interpolation algorithm}
}
\details{
This is not coded yet; the returned value is just the supplied value.
}
\value{
  A \code{section} object.  
}
\seealso{
Sections are normally created with \code{\link{read.section}} or \code{\link{make.section}}.
}
\examples{
\dontrun{
library(oce)
# File source -- http://cchdo.ucsd.edu/data_access?ExpoCode=90CT40_1
# a03 <- read.section("a03_hy1.csv")
data(a03)
a03g <- grid.section(a03, pressures=seq(0,2000,50))
data(coastline.world)
# Select the Gulf Stream region
plot(a03g,station.indices=seq(124,102),coastline=coastline.world,map.xlim=c(-80,-60))
}}
\references{
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}
