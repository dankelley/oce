\name{a03}
\docType{data}
\alias{a03}
\title{WOCE section a03}
\description{
This is line A03 (ExpoCode 90CT40\_1, with nominal sampling date
1993-09-11).  The chief scientist was Tereschenkov of SOI, working
aboard the Russian ship Multanovsky, undertaking a westward
transect from the Mediterranean outflow region across to North
America, with a change of heading in the last few dozen stations
to run across the nominal Gulf Stream axis.
}
\examples{
library(oce)
# Gulf Stream (note that section runs east to west)
data(a03)
Gulf.Stream <- section.subset(a03, 124:102)
Gulf.Stream.gridded <- section.grid(Gulf.Stream, p=seq(0, 5000, 10))
data(coastline.world)
plot(Gulf.Stream.gridded, coastline=coastline.world, map.xlim=c(-80,-60))
}
\usage{data(a03)}
\source{\url{http://cchdo.ucsd.edu/data_access?ExpoCode=90CT40_1} (use the \emph{BOT} link in the \emph{Exchange} section of this page.)}
\references{\url{http://cchdo.ucsd.edu}}
\seealso{This was read with \code{\link{read.section}("a03_hy1.csv")}, working a data file downloaded from the CCHDO site mentioned above.}
\author{Dan Kelley}
\keyword{misc}
