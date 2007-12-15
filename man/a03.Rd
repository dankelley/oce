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
# Gulf Stream
data(a03)
a03g <- section.grid(a03, p=seq(0,2000,10))
data(coastline.world)
# Select the Gulf Stream region
plot(a03g,station.indices=seq(124,102),coastline=coastline.world,map.xlim=c(-80,-60))
}
\usage{data(a03)}
\source{\url{http://cchdo.ucsd.edu/data_access?ExpoCode=90CT40_1} (use the \emph{BOT} link in the \emph{Exchange} section of this page.)}
\references{\url{http://cchdo.ucsd.edu}}
\seealso{This was read with \code{\link{read.section}(a03_hy1.csv)}, working a data file downloaded from the CCHDO site mentioned above.}
\author{Dan Kelley}
\keyword{misc}
