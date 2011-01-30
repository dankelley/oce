\name{a03}

\docType{data}

\alias{a03}

\title{WOCE section a03}

\description{This is line A03 (ExpoCode 90CT40_1, with nominal sampling
  date 1993-09-11).  The chief scientist was Tereschenkov of SOI,
  working aboard the Russian ship Multanovsky, undertaking a westward
  transect from the Mediterranean outflow region across to North
  America, with a change of heading in the last few dozen stations to
  run across the nominal Gulf Stream axis.}

\examples{
library(oce)
# Gulf Stream (note that section runs east to west)
data(a03)
Gulf.Stream <- subset(a03, indices=124:102)
Gulf.Stream.gridded <- section.grid(Gulf.Stream, p=seq(0, 5000, 100))
data(coastline.world)
plot(Gulf.Stream.gridded, coastline=coastline.world, map.xlim=c(-80,-60))
}

\usage{data(a03)}

\source{
This is based on a WOCE file downloaded from a server, as below:
\preformatted{
a03 <- read.section("http://cchdo.ucsd.edu/data/onetime/atlantic/a03/a03_hy1.csv", 
	            section.id="a03",
                    ship="R/V Professor Multanovskiy", 
		    scientist="Vladimir Tereschenkov",
		    institute="SIO")
save(a03, file="oce/data/a03.rda")
}
}

\seealso{See \code{\link{read.section}} for more on section-related
  functions and data sets.}

\author{Dan Kelley}

\keyword{misc}
