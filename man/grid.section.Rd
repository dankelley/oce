\name{grid.section}
\alias{grid.section}
\title{Grid a section}
\description{Grid a section, by interpolating to fixed pressure levels.}
\usage{grid.section(section, pressures=NA, quiet=TRUE)}
\arguments{
  \item{section}{A section to be gridded.}
  \item{pressures}{Pressure levels to which interpolation should be done.  If this is not supplied, the profiles will be examined and a vertical grid will be created that has approximately 100 levels at the deepest station.  If a single value is provided, it is taken as a step for a seq() running from 0 to the maximum pressure (rounded to the step).  If a list is given, it is used.  If \code{pressures} is set to the string \code{"levitus"}, then standard levels are used, as in the Levitus atlas.}
  \item{quiet}{Set FALSE to print some information that might be helpful during debugging.}
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
# http://cchdo.ucsd.edu/data_access?ExpoCode=90CT40_1
# ExpoCode 90CT40_1, Chief Scientist Tereschenkov/SOI
s <- read.section("a03_hy1.csv") 
s.gridded <- grid.section(s, "levitus")
plot(s.gridded)
}
}
\references{
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}
