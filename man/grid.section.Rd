\name{grid.section}
\alias{grid.section}
\title{Grid a section}
\description{Grid a section, by interpolating to fixed pressure levels.}
\usage{grid.section(section, pressures=NULL, algorithm=c("approx","smooth.spline"), ...)}
\arguments{
  \item{section}{A section to be gridded.}
  \item{pressures}{Pressure levels to which interpolation should be done.  If this is not supplied, the profiles will be examined and a vertical grid will be created that has approximately 100 levels at the deepest station.  If a single value is provided, it is taken as a step for a seq() running from 0 to the maximum pressure (rounded to the step).  If a list is given, it is used.  If \code{pressures} is set to the string \code{"levitus"}, then standard levels are used, as in the Levitus atlas.}
  \item{algorithm}{algorithm to use for interpolation, either the default \code{"approx"} to use the \code{\link{approx}} R function, or \code{"smooth.spline"} to use \code{\link{smooth.spline}} R function (see Details).}
  \item{...}{optional arguments to be supplied to the interpolation algorithm, e.g. \code{df} might be used with the \code{smooth.spline} method.}
}
\details{
The \code{"approx"} method uses \code{\link{approx}} to perform a linear interpolation between adjacent points.  Since this only uses data points that are immediate neighbors of prediction points, it has the disadvantage of discarding some data.  However, it has the strong advantage, compared with any interpolation scheme with which the author is familiar, of completely eliminating overshoots under all conditions.  The \code{"smooth.spline"} method calls \code{\link{smooth.spline}} to fit a smoothing spline.  This uses all the data, and produces smooth curves (which can make contours look better), but it overshoots in some circumstances.  The author has found, with the \code{a03} dataset, that the smoothing spline produces overshoots for about 10 percent of stations, yielding spurious patterns in contoured fields.  

It is probably best to start with \code{algorithm="approx"}.
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
